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
    const kill1 = new Howl({src: ['snd_kill_1.ogg']})
    const kill2 = new Howl({src: ['snd_kill_2.ogg']})
    const kill3 = new Howl({src: ['snd_kill_3.ogg']})
    return function (name) {
        switch (name) {
            case 'shoot' :
                shoot.play()
                break
            case 'hit' :
                hit.play()
                break
            case 'kill_1' :
                kill1.play()
                break
            case 'kill_2' :
                kill2.play()
                break
            case 'kill_3' :
                kill3.play()
                break
        }
    }
})())

