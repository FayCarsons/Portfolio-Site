import { render } from 'solid-js/web'
import { Router, Route } from '@solidjs/router'
import { lazy } from 'solid-js'
import './App.css'

import Metrics from 'metrics'

Metrics.logUserBackoff()

const Articles = lazy(() => import('./Components/Articles'));
const Home = lazy(() => import('./Components/Home'));
const Sketches = lazy(() => import('./Components/Sketches'));

render(
  () =>
    <Router>
      <Route path="/" component={Home} />
      <Route path="/articles/:title?" component={Articles} />
      <Route path="/sketches" component={Sketches} />
    </Router>
  ,
  document.getElementById('root')
)
