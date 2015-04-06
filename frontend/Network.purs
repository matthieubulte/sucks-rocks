module Network
    ( get
    , put
    ) where

import Control.Monad.Eff
import Control.Monad.Cont.Trans

foreign import getImpl """
function getImpl(url) {
  return function(callback) {
    return function() {
        var ajax = new XMLHttpRequest();
        ajax.onreadystatechange = function() {
            if (ajax.readyState == 4) {
               callback(ajax.response)();
            }
        };
        ajax.open("GET", url, true);
        ajax.send();
    };
  };
}
""" :: forall eff. String -> (String -> Eff eff Unit) -> Eff eff Unit

get :: forall eff. String -> ContT Unit (Eff eff) String
get url = ContT $ getImpl url

foreign import putImpl """
function putImpl(url) {
  return function(callback) {
    return function() {
        var ajax = new XMLHttpRequest();
        ajax.onreadystatechange = function() {
            if (ajax.readyState == 4) {
               callback(ajax.response)();
            }
        };
        ajax.open("PUT", url, true);
        ajax.send();
    };
  };
}
""" :: forall eff. String -> (String -> Eff eff Unit) -> Eff eff Unit

put :: forall eff. String -> ContT Unit (Eff eff) String
put url = ContT $ putImpl url


