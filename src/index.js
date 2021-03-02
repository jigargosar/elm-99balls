import "./styles.css"
// noinspection ES6CheckImport
import {Elm} from "./Main.elm"
// noinspection ES6UnusedImports
import {Howl, Howler} from 'howler';


const app = Elm["Main"].init({
    node: document.getElementById('root')
});


function mapValues(fn, obj) {
    return Object
        .keys(obj)
        .reduce(
            (acc, k) => Object.assign(acc, {[k]: fn(obj[k])}),
            {},
        )
}


app.ports["playSound"]["subscribe"]((function () {
    const soundPathMapping =
        {
            shoot: 'snd_swoosh.ogg',
            hit: 'snd_hit.ogg',
            kill_1: 'snd_kill_1.ogg',
            kill_2: 'snd_kill_2.ogg',
            kill_3: 'snd_kill_3.ogg',
            kill_4: 'snd_kill_4.ogg',
            kill_5: 'snd_kill_5.ogg',
            kill_6: 'snd_kill_6.ogg',
            kill_7: 'snd_kill_7.ogg',
            kill_8: 'snd_kill_8.ogg',
        }

    const sounds = mapValues((v) => new Howl({src: [v]}), soundPathMapping)


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
            default :
                console.warn("sound not found", name)
        }
    }
})())

