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
    const hit = new Howl({src: ['snd_hit.ogg']})
    return function (name) {
        switch (name){
            case 'shoot' :
                shoot.play()
                break
            case 'hit' :
                hit.play()
                break
        }
    }
})())

