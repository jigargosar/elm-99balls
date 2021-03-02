import "./styles.css"
// noinspection ES6CheckImport
import {Elm} from "./Main.elm"
// noinspection ES6UnusedImports
import {Howl, Howler} from 'howler';


const app = Elm["Main"].init({
    node: document.getElementById('root')
});


app.ports["playSound"]["subscribe"]((function () {
    const sound = new Howl({src: ['snd1.ogg']})
    return function () {
        sound.play()
    }
})())

