module Network
    ( get, get'
    , put, put'
    ) where

import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Network.HTTP
import Data.Array
import Data.Function

-- Internal tuple type
data JsHeader = JsHeader String String

foreign import sendImpl """
function sendImpl(method, url, headers, data, callback) {
    return function() {
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {
            if(request.readyState === 4) {
               callback(request.response)();
           }
        };

        for(var i=0; i<headers.length; i++) {
            request.setRequestHeader(headers[i].value0, headers[i].value1);
        }

        request.open(method, url, true);
        request.send(data);
    };
}
""" :: forall eff. Fn5 String String [JsHeader] String (String -> Eff eff Unit) (Eff eff Unit)

send :: forall eff. Verb -> String -> [Header] -> String -> ContT Unit (Eff eff) String
send verb url headers sendData = ContT $ runFn5 sendImpl (show verb) url (toJsHeader <$> headers) sendData
    where
        toJsHeader = \(Header h s) -> JsHeader (show h) s

get :: forall eff. String -> [Header] -> String -> ContT Unit (Eff eff) String
get = send GET

get' :: forall eff. String -> ContT Unit (Eff eff) String
get' url = get url [] ""

put :: forall eff. String -> [Header] -> String -> ContT Unit (Eff eff) String
put = send PUT

put' :: forall eff. String -> ContT Unit (Eff eff) String
put' url = put url [] ""
