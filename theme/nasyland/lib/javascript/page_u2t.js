! function () {
    var eles = document.querySelectorAll("#up2top")
    if (eles[1]) {
        ele = eles[1]
    } else {
        ele = eles[0]
    }
    ele.classList.add("page_up2top")
    var e = ele.querySelector("svg")
    if (e) {
        e.classList.add("fa-2x")
    } else {
        ele.querySelector("i").classList.add("fa-2x")
    }
    if (document.body.offsetHeight <= innerHeight) {
        ele.classList.add("hidden")
    }
}()
