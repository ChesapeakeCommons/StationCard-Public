function checkShinyBusy(mapLoad){
    if( mapLoad === true ){
        if( $('html').hasClass('shiny-busy') === true ){
         //   console.log ("I'm busy!");
            AddLoadBar();
        }else {
            removeLoadBar();
        }
    }
}

function AddLoadBar(){
    $('#loadBar_wrapper').css({
        'display' : 'block'
    });
}

function removeLoadBar(){
    $('#loadBar_wrapper').css({
        'display' : 'none'
    });
}

