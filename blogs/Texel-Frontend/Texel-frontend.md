---
title: Texel pt.2 - Effect on the frontend
date: 07-11-2025
tags: Effect, FP, Frontend, Design, Texel
---

Recently I've been working on [texel](https://faycarsons.xyz/articles/Texel-Intro), a ShaderToy successor with enhanced social features, a polished UI, and an emphasis on accessibility. Planning out the frontend I considered a handful of functional frontend languages or frameworks, and what I ended up choosing was [Effect.ts](https://effect.website) and [Solid](https://www.solidjs.com/). Much of the discussion around Effect I've encountered recently asserts that it is only suited for the backend, but I've had a largely positive experience with it and I wanted to share some of what that looks like.

I'm going assume that you understand how Effect works and why it and the functional concepts it's built on are valuable. If you're unfamiliar with it, I suggest starting with their [excellent documentation](effect.website/docs).

## What does it solve?

The biggest win is using one well-designed library of modular building blocks instead of a Zod + NeverThrow + Zustand and so on. Effect's modules follow the same patterns, so once you understand the core `Effect.Effect` module, you understand the rest.

After that, it's the functional idioms that I miss when I write TypeScript. I want immutable data, managed effects, sum types for error handling and state machines - and I'd prefer not to forgo TypeScript excellent tooling and ecosystem to get them. 

## What is the cost?
I personally found the learning curve negligible coming from OCaml and Haskell, but if you or your team don't have experience with *Typed Functional Programming*, then you may struggle to adopt it. You should probably understand what sum types, managed effects, and (I know) *monads* are.

For some portion of simpler, but still non-trivial, apps Effect is maybe overkill. Your React e-commerce site probably doesn't need an event bus, and maybe it doesn't need schema-based validation either, I don't know. I would argue, though, that the core patterns supplied by Effect will have a hugely positive impact on the quality of *any* codebase.

Most egregiously, it *will* inflate your bundle size. You can cut a lot out, but the runtime is still there and it is not small. That said, my app, which contains a load of WebGPU boilerplate and management as well as a whole code editor, is around 200kb gzipped - far from the worst.

## What does it look like?

 I think Effect's benefit is most clear in my HTTP client code. With a bit of boilerplate we can get something nice and composable for all our API calls. We create a `Client` service:
 
 ```typescript
export class Client extends Context.Tag('App.Client')<Client, HttpClient>() {}

export function make(): Effect.Effect<HttpClient, HttpClientError> {
  return Effect.gen(function*() {
    const baseClient = yield* HttpClient.HttpClient 

    return pipe(
	  // default client
      baseClient,
      // only accept 2xx responses as success
      HttpClient.filterStatusOk,
      HttpClient.mapRequestInput(HttpClientRequest.prependUrl('/api/')),
      // Do exponential backoff for all trivial errors 
      HttpClient.retryTransient({
        schedule: Schedule.exponential('50 millis', 2),
        times: 5
      }),
    )
  }).pipe(
	// Provide the default HTTP Client
    Effect.provide(FetchHttpClient.layer)
  )
}

// Provide a wrapper for GET requests
export function get(url: string): Effect.Effect<HttpClientResponse, HttpClientError, Client> {
  return Effect.andThen(Client, client => client.get(url))
}
```

And now we can define our API calls concisely, returning `Either` or `Option` for error handling, validating our data with a schema, chaining calls together:

 ```typescript
 export function isAuthorized(): Effect.Effect<
  Option.Option<Person>, 
  HttpClientError | ParseError,
  Client
> {
  return pipe(
    Client.get('myAuthEndpoint/isThisGuyAuthorized'),
    // Parse the response with our schema
    Effect.andThen(HttpClientResponse.schemaBodyJson(PersonSchema)),
    // Map success/failure to the `Option` type
    Effect.option
  )
}

export function login(email: string, password: string): Effect<User, LoginError | ParseError, Client> {
	return pipe(
		Client.post('login', { email, password }),
		Effect.mapError(LoginError.fromStatus),
		Schema.decodeUnknown(User.UserSchema)
	)
}
```

Now updating our global user state is pattern matching over the result of `isAuthorized`, We can get back a sum type enumerating login errors from `login`, and, most importantly, we know which functions are able to make requests or fail, and which aren't.

## Concurrency and global state

We have to execute these functions inside of an Effect context, though. There are a lot of ways we could achieve this, I chose to divide it by local state updates vs global state updates. 

The trivial case, fetching or updating local state looks like this:

```typescript
  const [sketches] = createResource(() =>
    Effect.runPromise(Api.mostPopularSketches()),
  )
``` 

Not super exciting, but it allows us to leverage all of Effect's benefits where we need them at the cost of an extra function call.

In the case of global state, things get more complicated. As mentioned, my app has a lot of complex content management and ensuring users cannot button-mash their way into some nonsense state is pretty important. 

If someone presses save while also editing and changing some other fundamental aspect of their content (you'd be surprised the messes you can make with just a Vim browser extension), those interactions must be handled atomically - in the sense that they are indivisible and cannot interleave. Without this anything can happen, pasting text into images, inserting a "foo" into the "bar" table, users with auth cookies labeled guests, total chaos! To solve this, I created an event bus powered by Effect's `Queue`. 

First, we define an event type which can contain one of three things:

- An effect which produces a *description* of a state update
- A stream of events
- An effect of type void, representing some work we want to happen in serial. Navigation, for example

We also probably want some event identifier for logging and telemetry. It looks like this: 

```typescript
type AppEvent<E> = {
	kind: string
	inner: Effect<StateUpdate | Stream<AppEvent<E>, E> | void, E>
}
```

And then we can push these events to a queue and run them:

```typescript
namespace EventBus {
	export type EventBus = {
		dispatch: <E>(event: AppEvent<E>) => void
	}
	
	export function make(state: State, setState: StoreSetterFunction<State>): Effect<EventBus> {
		return Effect.gen(function* () {
			let self = {}

			const eventQueue = yield* Queue.unbounded()

			function dispatch<E>(event: AppEvent<E>) {
				Queue.unsafeOffer(eventQueue, event)
			}

			function runEvent(event: AppEvent<E>) {
				return Effect.gen(function* () {
					const result = yield* event.inner

					if (StateUpdate.isStateUpdate(result)) {
						setState(result.field, result.value)	
					} else if (Stream.isStream(result)) {
						yield* Stream.runDrain(runEvent)
					}
				})
			}

			const services = Layer.mergeAll(
				Layer.succeed(State.Store, State.makeStore(state, setState)),
				Layer.effect(Client, Client.make())
			)

			pipe(
				Queue.take(eventQueue),
				Effect.tap(event => Effect.logDebug(`Got event: ${event.kind}`)),
				Effect.andThen(event => runEvent(event)),
				Effect.forever,
				Effect.provide(services),
				Effect.runFork
			)

			return { dispatch }
		})
	}
}
```

That's it. We pull effects from the queue and execute them in sequence. Each has to complete before we can move on to the next. In the event that concurrency *is* what we want, Effect's `withConcurrency` combinator can be used to configure this on a more granular level. For example, we could extend the event type to accept arrays of effect to communicate this to the event bus and execute it like this:

```typescript
const result = 
	yield* Array.isArray(event.inner) 
		? Effect.withConcurrency(Effect.all(event.inner), 'unbounded') 
		: event.inner
```

In conclusion, there are trade-offs and ultimately it may not be right for *your* frontend, but it can be a valuable tool for managing complexity and making your app more robust.

