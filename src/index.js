import "./styles.css"
// noinspection ES6CheckImport
import {Elm} from "./Main.elm"
// noinspection ES6UnusedImports
import {Howl, Howler} from 'howler';


const app = Elm["Main"].init({
    node: document.getElementById('root'),
    flags: {
        stars: Number.parseInt(localStorage.getItem("stars")) || 0
    }
});


function mapValues(fn, obj) {
    return Object
        .keys(obj)
        .reduce(
            (acc, k) => Object.assign(acc, {[k]: fn(obj[k], k)}),
            {},
        )
}

app.ports["saveStars"]["subscribe"]((function (stars) {
    localStorage.setItem("stars", stars)
}))

app.ports["playSound"]["subscribe"]((function () {
    const soundPathMapping =
        {
            shoot: 'snd_shoot.ogg',
            hit: 'snd_hit.ogg',
            bonus_hit: 'snd_bonus_hit.ogg',
            kill_1: 'snd_kill_1.ogg',
            kill_2: 'snd_kill_2.ogg',
            kill_3: 'snd_kill_3.ogg',
            kill_4: 'snd_kill_4.ogg',
            kill_5: 'snd_kill_5.ogg',
            kill_6: 'snd_kill_6.ogg',
            kill_7: 'snd_kill_7.ogg',
            kill_8: 'snd_kill_8.ogg',
        }

    const soundMap = mapValues(
        (v, k) => {
            return new Howl({
                src: [v],
                volume: k.startsWith("kill_") ? 0.3 : 1
            });
        },
        soundPathMapping,
    )

    return function playSound(name) {
        const sound = soundMap[name]
        if (sound) {
            sound.play()
        } else {
            console.warn("sound not found", name)
        }

    }
})())

