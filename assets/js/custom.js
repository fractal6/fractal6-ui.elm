
/*
 *
 * Utils
 *
 */

const toggleTheme = () =>  {
    var is_dark = document
        .getElementById('cssTheme')
        .classList[0] === 'dark_theme';

    var element = document.getElementById('cssTheme');

    if (is_dark) {
        element.href = '/light.css';
        element.classList.remove('dark_theme')
        element.classList.add('light_theme');
    } else {
        element.href = '/dark.css';
        element.classList.remove('light_theme')
        element.classList.add('dark_theme');
    }
}
