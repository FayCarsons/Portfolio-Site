%%raw("import './index.css'")

type f = Foo(string, int) | Bar | Baz

let is_bar = switch user {
| Bar => true
| _ => false
}
