/*
 * The main javascript file of this blog
 */
// normal functions

/*
 * Back to top smoothly
 */
function scroll_to(element, to) {
    var difference = to - element.scrollTop;

    if (Math.abs(difference) < 10) {
        element.scrollTop = to;
        return;
    }


    var ratio = 5;
    element.scrollTop = (element.scrollTop * ratio + to) / (ratio + 1);

    setTimeout(function () {
        scroll_to(element, to);
    }, 10);
}
// function scroll_to(element, to, duration) {
//     if (duration < 0) return;
//     var difference = to - element.scrollTop;
//     var perTick = difference / duration * 2;
//
//     setTimeout(function () {
//         element.scrollTop = element.scrollTop + perTick;
//         scroll_to(element, to, duration - 2);
//     }, 10);
// }
document.querySelectorAll('#up2top').forEach(function (e) {
    e.onclick = function () {
        var high = document.body.scrollTop
        if (high / 100 > 100) {
            // scroll_to(document.body, 0, high / 100)
            scroll_to(document.body, 0, 100)
        } else {
            scroll_to(document.body, 0, 100)
        }
    }
})




/*
 * Show and fade out.
 */
function showe(e) {
    var ele = document.querySelector(e)
    ele.style.display = "block"
    ele.style.opacity = 1
}

function fade_out(e) {
    var ele = document.querySelector(e)
    if (ele.style.opacity === "") {
        ele.style.opacity = 1
    }
    if (ele.style.opacity > 0.2) {
        setTimeout(function () {
            ele.style.opacity = ele.style.opacity - 0.01
            fade_out(e)
        }, 10);
    } else {
        ele.style.display = "none"
    }
}

function hide(e) {
    var ele = document.querySelector(e)
    ele.style.opacity = 1
    ele.style.display = "none"
}

function toggle_sf(e) {
    var ele = document.querySelector(e)
    if (getComputedStyle(ele).display != "none") {
        fade_out(e)
    } else {
        showe(e)
    }
}

/*
 * onscroll
 */
function fade_out_toc() {
    if (innerWidth <= 768) {
        var ele = document.querySelector("#toc")
        if (ele) {
            if (getComputedStyle(ele).display != "none") {
                fade_out("#toc")
            }
        }
    }
}

function onscroll_fade_out_toc() {
    if (addEventListener)
        addEventListener('scroll', fade_out_toc, false);
    else if (el.attachEvent)
        attachEvent('onscroll', fade_out_toc);
}

// domready functions
function formattime() {
    document.querySelectorAll("time[datetime]").forEach(function (e) {
        e.textContent = moment(e.attributes.datetime.textContent).format("ll")
        console.log(moment(e.attributes.datetime.textContent).format("ll"))
    })
}

function add_icon() {
    document.querySelectorAll("h1").forEach(function (e) {
        var ele_i = document.createElement("i")
        ele_i.className = "fas fa-star h1_icon"
        e.prepend(ele_i)
        e.classList.add("no-before")
    })
    document.querySelectorAll("h2").forEach(function (e) {
        var ele_i = document.createElement("i")
        ele_i.className = "far fa-star h2_icon"
        e.prepend(ele_i)
        e.classList.add("no-before")
    })
    document.querySelectorAll("h3").forEach(function (e) {
        var ele_i = document.createElement("i")
        ele_i.className = "far fa-star-half h3_icon"
        e.prepend(ele_i)
        e.classList.add("no-before")
    })
    document.querySelectorAll("h4").forEach(function (e) {
        var ele_i = document.createElement("i")
        ele_i.className = "far fa-star-half h4_icon"
        e.prepend(ele_i)
        e.classList.add("no-before")
    })
}

function domready() {
    // Edited from https://github.com/ded/domready v1.0.8
    var fns = [],
        listener, hack = document.documentElement.doScroll,
        loaded = (hack ? /^loaded|^c/ : /^loaded|^i|^c/).test(document.readyState)

    if (!loaded && document)
        document.addEventListener("DOMContentLoaded", listener = function () {
            document.removeEventListener("DOMContentLoaded", listener)
            loaded = 1
            while (listener = fns.shift()) listener()
        })

    return function (fn) {
        loaded ? setTimeout(fn, 0) : fns.push(fn)
    }
}

domready(formattime(), add_icon(), onscroll_fade_out_toc())
