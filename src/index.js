// @ts-ignore
import Elm from './Main.elm';
import './styles/index.css';


function toggleDarkMode() {
    const htmlElement = document.getElementsByTagName('html')[0];
    if (htmlElement.classList.contains('dark')) {
        htmlElement.classList.remove('dark');
    } else {
        htmlElement.classList.add('dark');
    }
}

const app = Elm.Main.init({
    node: document.getElementById('app'),
});

app.ports.sendMessage.subscribe(function (message) {
    switch (message) {
        case 'toggle-dark-mode':
            toggleDarkMode();
            break;
        default:
            break;
    }
});