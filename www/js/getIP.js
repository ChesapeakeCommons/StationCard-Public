      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.unbindAll();
          Shiny.onInputChange("geolocation", false);
          Shiny.bindAll();
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            Shiny.unbindAll();
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.setInputValue("geolocation", true);
            Shiny.setInputValue("latitude", coords.latitude);
            Shiny.setInputValue("longitude", coords.longitude);
            Shiny.bindAll();
          }, 1100)
        
        }
      });