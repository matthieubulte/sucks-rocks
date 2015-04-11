module Network
    ( Response()
    , get, get'
    , put, put'
    , post, post'
    ) where

import Control.Monad.Eff
import Control.Monad.Cont.Trans
import Network.HTTP
import Data.Array
import Data.Function

-- Internal type only used to ease JS interfacing
data JsHeader = JsHeader String String

-- A *good enough* reponse type
type Response = { text   :: String
                , status :: Number
                }

foreign import sendImpl """
function sendImpl(method, url, headers, data, callback) {
    return function() {
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {
            if(request.readyState === 4) {
               callback({
                   text:   request.responseText,
                   status: request.status
               })();
           }
        };

        for(var i=0; i<headers.length; i++) {
            request.setRequestHeader(headers[i].value0, headers[i].value1);
        }

        request.open(method, url, true);
        request.send(data);
    };
}
""" :: forall eff. Fn5 String String [JsHeader] String (Response -> Eff eff Unit) (Eff eff Unit)

send :: forall eff. Verb -> String -> [Header] -> String -> ContT Unit (Eff eff) Response
send verb url headers sendData = ContT $ runFn5 sendImpl (show verb) url (toJsHeader <$> headers) sendData
    where
        toJsHeader = \(Header h s) -> JsHeader (show h) s

get :: forall eff. String -> [Header] -> String -> ContT Unit (Eff eff) Response
get = send GET

get' :: forall eff. String -> ContT Unit (Eff eff) Response
get' url = get url [] ""

put :: forall eff. String -> [Header] -> String -> ContT Unit (Eff eff) Response
put = send PUT

put' :: forall eff. String -> ContT Unit (Eff eff) Response
put' url = put url [] ""

post :: forall eff. String -> [Header] -> String -> ContT Unit (Eff eff) Response
post = send POST

post' :: forall eff. String -> ContT Unit (Eff eff) Response
post' url = post url [] ""


