



var storage = window.sessionStorage;

var windowHeight = $(window).height();
var windowWidth = $(window).width();
var aspectRatio = getAspectRatio();
var isLandscape = getIsLandscape();
var footerHeight = $('#footer').find('a').innerHeight();
var mainHeight = $('#main').innerHeight();
var mapHeight = $('#Map').innerHeight();
var headerHeight = $('#main_header').innerHeight();

var mapLoadedFlag = false;
var mapAttrRemovedFlag = false;
var verticalMarginLayoutFlag = false;

var nextPrevMoved = false;
var qrModalPopulated = false;
var infoMoved = false;

function setViewVars() {

    windowHeight = $(window).height();
    windowWidth = $(window).width();
    isLandscape = getIsLandscape();
    aspectRatio = getAspectRatio();
  //  infoMoved = false;

    footerHeight = $('#footer').find('a').innerHeight();
    mainHeight = $('#main').innerHeight();
    mapHeight = $('#Map').innerHeight();
    headerHeight = $('#main_header').innerHeight();

    // headerHeight excluded from available height calc because included in mapHeight outerheight top margin.
    availableScreenHeight = windowHeight - (headerHeight + mapHeight + mainHeight + footerHeight);


    storage.setItem('windowHeight', windowHeight);
    storage.setItem('windowWidth', windowWidth);
    storage.setItem('isLandscape', isLandscape);
    storage.setItem('aspectRatio', aspectRatio);
}

// return Boolean
function getIsLandscape() {
    if (windowWidth > windowHeight) {
        return true;
    } else {
        return false

    }

}

function getAspectRatio() {

    if (windowWidth > windowHeight) {

        aspectRatio = windowWidth / windowHeight;

    } else {

        aspectRatio = windowHeight / windowWidth;

    }

    return aspectRatio;
}


// Define a new var, set and 'type' to 'decorated' function
var viewUpdate = function (param02 = false,
                           param03 = false,
                           param04 = false
) {
  //  console.log('viewUpate defined as function');

};


// Okay, here is a function to reset the bg layer height to inner.width
// This is to prevent the mobile address bar from moving the loader image
function backgroundHeight(){
  $('#stationCard_background').css('height', window.innerHeight);
  $('#starterLogo_wrapper').css('height', window.innerHeight);
}

// windowHeight/windowWidth
// Define 'decorater' function. First param is name of ornement
// follow params must be used in order and values respect first param method def

window.onresize = function() {

    infoMoved = false;
    nextPrevMoved = false;
    
    backgroundHeight();

}

function viewFunctions(calledFunc,
                       param02 = false,
                       param03 = false,
                       param04 = false
) {


    //Create object of methods/functions that can be 
    //referenced in veiwFunction->calledFunc
    var funcs = {

        copyShinyLoader : function () {

       //     console.log('copyShinyLoader');
            $("div[class*='load1']" ).clone().appendTo('#loadBar_wrapper');

        },

        onTick : function() {

            setTimeout(function(){

                invertDropDown();

                checkShinyBusy(mapLoadedFlag);

                viewFunctions('onTick');

                viewFunctions('placeCredit');

                backgroundHeight();
              
            }, 100);

        },

        placeCredit : function(){
//           console.log('blue');

    //       console.log($(document).height() +' -- '+$(window).height());

            if( $(document).height() > $(window).height() ){

                $('#footer').css({

                    'position' : 'relative'

                });

            }else{
                $('#footer').css({

                    'position' : 'absolute'

                });
            }
        },

        enableDropDown : function(){

          //  console.log("3333333333")

        },

        heightSetter : function (param02 = false) {

            setViewVars();


         /*   $('#TrendsTitle').find('h4').css({
                        'display':'none'
                    });
                    
             $('#GaugeTitle').find('h4').css({
                        'display':'none'
              });
           */         
                    

            if(mapLoadedFlag){

               $('#stationCard_background').css({

                   'background-color' : 'white'

               });

               $('body').css({

                   'background-color' : 'white'

               });

            }

            if(mapLoadedFlag === true && mapAttrRemovedFlag === false){

                $('#Map').removeAttr("style");
           //     $('#Map').removeAttr("width");

                mapAttrRemovedFlag = true;
            }

            if (windowWidth >= 1025 && nextPrevMoved == false) {

                $('#NextStationLink').prependTo('#NextPrevConatiner');
                $('#PrevStationLink').prependTo('#NextPrevConatiner');
                $('#StationNameText').appendTo('#StationNameWrapper');
                $('.leaflet-top').attr('class', 'leaflet-top leaflet-right');

                nextPrevMoved = true;

                $('#status_container').appendTo('#main_info');
                $('#param_select_containter').appendTo('#main_info');
                $('#text_container').appendTo('#main_info');

                $('#main_tabset').appendTo('#main');

                $('#tablet_col_l').css({
                    'width' : 'unset',
                    'max-width' : 'unset'
                })
                $('#tablet_col_r').css({
                    'width' : 'unset',
                    'max-width' : 'unset'

                })
                
                $('.tab-inline_title').css({
                  'display' : 'block',
                  'margin-left' : '8px'
                });

                infoMoved = false;

            }


            if (windowWidth >= 1025 && $('#GaugeTitle').find('h4').css('min-height') != '20px') {

                if ($('#GaugeTitle').find('h4').text().length > 0) {

                    $('.svg-container').css({
                        'display': 'block'
                    });

                 //   $('#TrendsTitle').before('<h4>TRENDY</h4)');

                    $('#TrendsTitle').find('h4').html('<span style="font-size: 14px;">' + $('#TrendsTitle').find('h4').text() + '</span>').css({
                        'min-height': '20px'
                    });
                    
                    $('#GaugeTitle').find('h4').html('<span style="font-size: 14px;">' + $('#GaugeTitle').find('h4').text() + '</span>').css({
                        'min-height': '20px'
                    });

                } else {

                    $('.svg-container').css({'display': 'none'});
                    
                     $('.tab-inline_title').css({
                        'display' : 'block',
                        'margin-left' : '8px'
                        
                      })

  
                }
                
                
            }


            if (windowWidth <= 1024) {

                $('.svg-container').css({
                    'display': 'block',
                    'width': '100%'
                });
                
                $('.tab-inline_title').css({
                        'display' : 'none'
                      })


            }

            if ((windowWidth <= 1024) && (windowWidth >= 640) && (infoMoved == false)){

                $('.leaflet-top').attr('class', 'leaflet-top leaflet-right');

                $('#PrevStationLink').prependTo('#NextPrevConatiner');
                $('#NextStationLink').prependTo('#NextPrevConatiner');

                $('#StationNameWrapper').appendTo('#tablet_col_l');
                $('#status_container').appendTo('#tablet_col_l');
                $('#param_select_containter').appendTo('#tablet_col_l');
                $('#text_container').appendTo('#tablet_col_l');

                $('#main_tabset').appendTo('#tablet_col_r');

                $('#tablet_col_l').css({
                    'width' : '59%',
                    'max-width' : '600px'
                })
                $('#tablet_col_r').css({
                    'width' : '39%',
                    'max-width' : '400px'

                })

                infoMoved = true;

            }

            if (windowWidth < 640 && nextPrevMoved === true){

                $('#NextStationLink').insertAfter('#Map');
                $('#PrevStationLink').insertAfter('#Map');
                $('#StationNameText').appendTo('#StationNameWrapper');

                nextPrevMoved = false;

                $('#status_container').appendTo('#main_info');
                $('#param_select_containter').appendTo('#main_info');
                $('#text_container').appendTo('#main_info');

                $('#main_tabset').appendTo('#main');

                $('#tablet_col_l').css({
                    'width' : 'unset',
                    'max-width' : 'unset'
                })
                $('#tablet_col_r').css({
                    'width' : 'unset',
                    'max-width' : 'unset'

                })

                infoMoved = false;

            }

            if (windowWidth <= 640 && MapExpanded === false){

                $('#StationNameSubWrapper').attr('onclick','mapExpand()')
                $('#mapExpand').attr('onclick','mapExpand()')

            }else if (windowWidth <= 640 && MapExpanded === true){

                $('#StationNameSubWrapper').attr('onclick','mapCollapse()');
                $('#mapExpand').attr('onclick','mapCollapse()')



               // $('.leaflet-control-zoom, .leaflet-bar, .leaflet-control').appendTo('.leaflet-bottom, .leaflet-right');

            }else{

            }

/*

             $('#TrendsTitle').find('h4').css({
                        'display':'block'
                    });
                    
             $('#GaugeTitle').find('h4').css({
                        'display':'block'
              });
              */
            //Here we callback this function again after a setTimeout
            //This is in case the view needs updating.
            // ** need to add Session Flag Var to check for changes
            // ** and bypass resize logic.
            let callback = function callViewFunction() {

                //    window.onresize = function() {
                       viewFunctions('heightSetter');
                        viewFunctions('populateQRModal');
                        viewFunctions('spinnerColor');
                    //    viewFunctions('paramSelectObjectAdjuster');
                        
                //    }



            }

            setTimeout(callback, 500);


        },

        spinnerColor : function() {

           // console.log("Marshmallow !");

            $( ".loader" ).css({
                'color': 'hsla(194, 70%, 52%, 1.0)',
                'background': 'hsla(194, 70%, 52%, 1.0)'
            });

            $( ".loader" ).css({
                'color': 'hsla(194, 70%, 52%, 1.0)',
                'background': 'hsla(194, 70%, 52%, 1.0)'
            });
        },

        populateQRModal: function () {

            $(".modal-content").attr("id","#ModalContent");

            if($('#ModalContent').length == 0) {

            }else{

                qrModalPopulated = true;

           }

        },

        moveQRLink: function () {

            $(".nav-tabs").append("<li id='qr-tab'></li>");

            $("#qr-icon").height("30px").width("30px");

            $('#MoreInfoLink').appendTo('#qr-tab');

        },

        removeElements: function (text, selector) {

            var wrapped = $(text);

            wrapped.find(selector).remove();

            return wrapped.html();
        },

        showContent: function () {

            $('#stationCard_background').css('z-index', '250');

        },

        hideContent: function () {

            $('#stationCard_background').css('z-index', '1000');
            $('#stationCard_main').css('z-index', '500');

        },

        setMapLoadFlag: function () {

            mapLoadedFlag = !mapLoadedFlag;

        },
        
        paramSelectObjectAdjuster: function(){
          

          $('#TrendsTitle').before('<h4 class="tab-inline_title">Trends</h4)');
          $('#GaugeTitle').before('<h4 class="tab-inline_title">Latest Measurement</h4)')

        /*  
          $('#ParamSelect-selectized').change(function(){ 
            console.log('Apples or Banannas ?');
            
          });
          
           $('#ParamSelect-selectized').on('click',function(){ 
             
                console.log('Blue Bananas ?');
           });
           
           $('.item').on('change',function(){
             
             console.log('Change, is constantly staying the same.');
             
           })
           */
         //  function Barf(){
          //   console.log('yeah!');
          // }
           
        //   $('#ParamSelect-selectized').attr('onClick', 'Barf()');
         
            
        
          
        },

        colorSwap: function (param02 = false) {

            let domE = $('#GroupName');

            let callback = function callViewFunction() {

                viewFunctions('colorSwap');

            };

            if (domE.css("color") === "rgb(255, 0, 0)") {
         //       console.log('blue!');
                domE.css("color", "blue");
                setTimeout(callback, 1000);
            } else {
        //        console.log('red!');
                domE.css("color", "red");
                setTimeout(callback, 1000);
            }

        },

    };

    //set 'decorated'/'chamilion' function var to object member
    viewUpdate = funcs[calledFunc];

    // call 'decorated'/'chamilion' functon
    viewUpdate(param02, param03, param04);
}



