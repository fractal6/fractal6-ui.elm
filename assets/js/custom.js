
/*
 *
 * Utils
 *
 */

export function shadeColor(color, percent) {

    var R = parseInt(color.substring(1,3),16);
    var G = parseInt(color.substring(3,5),16);
    var B = parseInt(color.substring(5,7),16);

    R = parseInt(R * (100 + percent) / 100);
    G = parseInt(G * (100 + percent) / 100);
    B = parseInt(B * (100 + percent) / 100);

    R = (R<255)?R:255;
    G = (G<255)?G:255;
    B = (B<255)?B:255;

    var RR = ((R.toString(16).length==1)?"0"+R.toString(16):R.toString(16));
    var GG = ((G.toString(16).length==1)?"0"+G.toString(16):G.toString(16));
    var BB = ((B.toString(16).length==1)?"0"+B.toString(16):B.toString(16));

    return "#"+RR+GG+BB;
}

export function setpixelated(ctx2d, v) {
    ctx2d['imageSmoothingEnabled'] = v;       /* standard */
    ctx2d['oImageSmoothingEnabled'] = v;      /* Opera */
    ctx2d['webkitImageSmoothingEnabled'] = v; /* Safari */
    ctx2d['msImageSmoothingEnabled'] = v;     /* IE */
    //ctx2d['mozImageSmoothingEnabled'] = v;    /* Firefox (deprecated) */
}


const toggleTheme = () => {
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

// sleep time expects milliseconds
// usage: sleep(1000).then(() => {...})
export function sleep (time) {
    return new Promise((resolve) => setTimeout(resolve, time));
}
