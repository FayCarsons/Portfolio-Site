import { render } from 'solid-js/web'
import { Router, Route } from '@solidjs/router'
import { lazy } from 'solid-js'
import './App.css'

//import { Home} from './Components/Home'
//import { Articles } from './Components/Articles'


const Articles = lazy(() => import('./Components/Articles'));
const Home = lazy(() => import('./Components/Home'));

render(
    () => 
        <Router>
            <Route path="/" component={Home}></Route>
            <Route path="/articles/:title?" component={Articles}></Route>
        </Router>
    ,
    document.getElementById('root')
)