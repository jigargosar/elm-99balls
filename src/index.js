import "./styles.css"
import {Elm} from "./Main.elm"
// noinspection ES6UnusedImports
import {Howl, Howler} from 'howler';


const app = Elm.Main.init({node: document.getElementById('root')});

const sound = new Howl({
    src: ['snd1.ogg']
})

app.ports.playSound.subscribe(function () {
    sound.play()
})