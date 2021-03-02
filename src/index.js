import "./styles.css"
// noinspection ES6CheckImport
import {Elm} from "./Main.elm"
// noinspection ES6UnusedImports
import {Howl, Howler} from 'howler';


const app = Elm["Main"].init({
    node: document.getElementById('root')
});

app.ports["playSound"]["subscribe"]((function () {
    const shoot = new Howl({src: ['snd_swoosh.ogg']})
    return function () {
        shoot.play()
    }
})())

