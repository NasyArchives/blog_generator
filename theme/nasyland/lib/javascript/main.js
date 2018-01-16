$("#menu-icon").click(function() {
    if ($('#toc').css("display") == 'none') {
        $('#toc').show()
    } else {
        $('#toc').fadeOut();
    }
})

$(window).scroll(function() {
    if ($(window).width() < 768) {
        $('#toc').fadeOut();
    }
})

$(document).ready(function() {
    $("time[datetime]").text(moment($("time[datetime]").attr("datetime")).format("ll"))
})
