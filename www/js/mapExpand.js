var MapExpanded = false;

function mapExpand(){

 //   console.log('MapExpand --> MapExpand');

    let nh = $(window). height() - ( $('#StationNameWrapper').height()+$('.status_item').height()+20 );
    let newHeight = nh+'px';

    $('#text_container').css('display', 'none');
    $('#main_tabset').css('visibility', 'hidden');
    $('#footer').css('display', 'none');
    $('#mapExpand').css('display', 'none');
     $('#mapExpand').css({
              
                'visibility' : 'hidden'

            });
    

    $('#Map').animate({height:newHeight}, 500,
        function(){

           $(".selectize-dropdown, .single, .plugin-selectize-plugin-a11y").eq(1).attr('id','DropDown');

            map.invalidateSize();

            let mxp         = $('#Map').height() - 35;
            let mapExPos    = mxp + 'px';
            let collapsePos = mxp - 78;

            $('#mapExpand').css({
               //     'top'       :   mapExPos,
                    'display'   :   'block',
                'background-image' :   "url('../svg/fullscreen_exit_wrblue_24dp.svg')",
                'top'       : collapsePos+'px',
                'visibility' : 'visible'

            });

            $('.leaflet-control-zoom').css({'display'   : 'block',
                                            'top'       : '40px'
                                        });

            $('.leaflet-control-layers').css({'display' : 'block'
                                            });

           

            MapExpanded = true;

        }
    );
}

function invertDropDown() {

    if (MapExpanded === true) {

        setTimeout(function () {

            $('#DropDown').css({'top': 'unset', 'bottom': '43px'})

        }, 0);

    }

}

function mapCollapse(){

  //  console.log('MapExpand --> MapCollapse ');


    $('.leaflet-control-zoom').css('display', 'none');
    $('.leaflet-control-layers').css('display', 'none');
    

    $('#mapExpand').css({
            
                'visibility' : 'hidden'

            });


    let nh =    ( $(window). height() / 4)
             +  ( $('#main_header').height() );

    let newHeight = nh+'px';

    $('#Map').animate({height:newHeight}, 500,

        function(){

            $(".selectize-dropdown, .single, .plugin-selectize-plugin-a11y").eq(1).removeAttr('DropDown');

            map.invalidateSize();

            $('#text_container').css('display', 'block');
            $('#main_tabset').css('visibility', 'visible');
            $('#main_tabset').css('width', '100%');
            $('#footer').css('display', 'block');

            let mxp         = $('#Map').height() - 35;
            let mapExPos    = mxp + 'px';

            $('#mapExpand').css({
                'top'       :   mapExPos,
                'display'   :   'block',
                'background-image' :   "url('../svg/fullscreen_wrblue_24dp.svg')",
                  'visibility' : 'visible'
                

            });

            MapExpanded = false;

        }

    );
}