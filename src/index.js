import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';


var sound = new Audio('https://raw.githubusercontent.com/mark-chimes/trading-post/master/src/resources/bicycle-bell.mp3');
sound.loop = true;
var shouldPlaySounds = true;

const app = Elm.Main.init({
    node: document.getElementById('root'),
    flags: { windowWidth: window.innerWidth }
});

// app.ports.logger.subscribe(message => {
//     console.log('Port emitted a new message: ' + message);
// });

app.ports.startSound.subscribe(soundIndex => {
    if (shouldPlaySounds) {
        sound.play();
    }
});

app.ports.stopSound.subscribe(soundIndex => {
    sound.pause();
});

app.ports.setShouldPlay.subscribe(shouldPlay => {
    sound.pause();
    shouldPlaySounds = shouldPlay;
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
