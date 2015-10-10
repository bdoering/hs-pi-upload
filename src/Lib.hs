{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( myApp
    ) where

import Control.Applicative ((<$>), optional)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import Text.RawString.QQ (r)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Upload
    
myApp :: ServerPart Response
myApp = msum
  [ dir "echo"    $ echo
  -- , dir "query"   $ queryParams
  -- , dir "form"    $ formPage
  -- , dir "fortune" $ fortune
  , dir "static"   $ fileServing
  , dir "upload"  $ upload
  , dir "upload-target" $ uploadTarget
  , homePage
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage =
    ok $ template "home page" $ do
           H.h1 "Hello!"
           H.p "Writing applications with happstack-lite is fast and simple!"
           H.p "Check out these killer apps."
           H.p $ a ! href "/echo/secret%20message"  $ "echo"
           H.p $ a ! href "/query?foo=bar" $ "query parameters"
           H.p $ a ! href "/form"          $ "form processing"
           H.p $ a ! href "/fortune"       $ "(fortune) cookies"
           H.p $ a ! href "/files"         $ "file serving"
           H.p $ a ! href "/upload"        $ "file uploads"

echo :: ServerPart Response
echo =
    path $ \(msg :: String) ->
        ok $ template "echo" $ do
          p $ "echo says: " >> toHtml msg
          p "Change the url to echo something else."

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing [""] "/home/bd/src/hs-pi-upload/static"
            
upload :: ServerPart Response
upload = ok . toResponse $ do 
  H.preEscapedText [r|
  <!DOCTYPE html>
<html>
  <head>
    <title>Resumable.js - Multiple simultaneous, stable and resumable uploads via the HTML5 File API</title>
    <meta charset="utf-8" />
    <link rel="stylesheet" type="text/css" href="/static/css/upload.css" />
  </head>
  <body>
    <div id="frame">

      <h1>Resumable.js</h1>
      <p>It's a JavaScript library providing multiple simultaneous, stable and resumable uploads via the HTML5 File API.</p>

      <p>The library is designed to introduce fault-tolerance into the upload of large files through HTTP. This is done by splitting each files into small chunks; whenever the upload of a chunk fails, uploading is retried until the procedure completes. This allows uploads to automatically resume uploading after a network connection is lost either locally or to the server. Additionally, it allows for users to pause and resume uploads without loosing state.</p>

      <p>Resumable.js relies on the HTML5 File API and the ability to chunks files into smaller pieces. Currently, this means that support is limited to Firefox 4+ and Chrome 11+.</p>

      <hr/>

      <h3>Demo</h3>
      <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>
      <script src="/static/js/resumable.js"></script>

      <div class="resumable-error">
        Your browser, unfortunately, is not supported by Resumable.js. The library requires support for <a href="http://www.w3.org/TR/FileAPI/">the HTML5 File API</a> along with <a href="http://www.w3.org/TR/FileAPI/#normalization-of-params">file slicing</a>.
      </div>

      <div class="resumable-drop" ondragenter="jQuery(this).addClass('resumable-dragover');" ondragend="jQuery(this).removeClass('resumable-dragover');" ondrop="jQuery(this).removeClass('resumable-dragover');">
        Drop  files here to upload or <a class="resumable-browse"><u>select from your computer</u></a>
      </div>
      
      <div class="resumable-progress">
        <table>
          <tr>
            <td width="100%"><div class="progress-container"><div class="progress-bar"></div></div></td>
            <td class="progress-text" nowrap="nowrap"></td>
            <td class="progress-pause" nowrap="nowrap">
              <a href="#" onclick="r.upload(); return(false);" class="progress-resume-link"><img src="/static/images/resume.png" title="Resume upload" /></a>
              <a href="#" onclick="r.pause(); return(false);" class="progress-pause-link"><img src="/static/images/pause.png" title="Pause upload" /></a>
            </td>
          </tr>
        </table>
      </div>
      
      <ul class="resumable-list"></ul>

      <script>
        var r = new Resumable({
            target:'/upload-target',
            chunkSize:1*1024*1024,
            simultaneousUploads:4,
            testChunks: true,
            throttleProgressCallbacks:1,
            method: "octet"
          });
        // Resumable.js isn't supported, fall back on a different method
        if(!r.support) {
          $('.resumable-error').show();
        } else {
          // Show a place for dropping/selecting files
          $('.resumable-drop').show();
          r.assignDrop($('.resumable-drop')[0]);
          r.assignBrowse($('.resumable-browse')[0]);
          // Handle file add event
          r.on('fileAdded', function(file){
              // Show progress pabr
              $('.resumable-progress, .resumable-list').show();
              // Show pause, hide resume
              $('.resumable-progress .progress-resume-link').hide();
              $('.resumable-progress .progress-pause-link').show();
              // Add the file to the list
              $('.resumable-list').append('<li class="resumable-file-'+file.uniqueIdentifier+'">Uploading <span class="resumable-file-name"></span> <span class="resumable-file-progress"></span>');
              $('.resumable-file-'+file.uniqueIdentifier+' .resumable-file-name').html(file.fileName);
              // Actually start the upload
              r.upload();
            });
          r.on('pause', function(){
              // Show resume, hide pause
              $('.resumable-progress .progress-resume-link').show();
              $('.resumable-progress .progress-pause-link').hide();
            });
          r.on('complete', function(){
              // Hide pause/resume when the upload has completed
              $('.resumable-progress .progress-resume-link, .resumable-progress .progress-pause-link').hide();
            });
          r.on('fileSuccess', function(file,message){
              // Reflect that the file upload has completed
              $('.resumable-file-'+file.uniqueIdentifier+' .resumable-file-progress').html('(completed)');
            });
          r.on('fileError', function(file, message){
              // Reflect that the file upload has resulted in error
              $('.resumable-file-'+file.uniqueIdentifier+' .resumable-file-progress').html('(file could not be uploaded: '+message+')');
            });
          r.on('fileProgress', function(file){
              // Handle progress for both the file and the overall upload
              $('.resumable-file-'+file.uniqueIdentifier+' .resumable-file-progress').html(Math.floor(file.progress()*100) + '%');
              $('.progress-bar').css({width:Math.floor(r.progress()*100) + '%'});
            });
        }
      </script>

    </div>
  </body>
</html> |]


uploadTarget :: ServerPart Response
uploadTarget =
    msum [ uploadTargetGet
         , uploadTargetPost
         ]

-- > upload :: ServerPart Response
-- > upload =
-- >        msum [ uploadForm
-- >             , handleUpload
-- >             ]
-- >     where
-- >     uploadForm :: ServerPart Response
-- >     uploadForm =
-- >         do method GET
-- >            ok $ template "upload form" $ do
-- >              form ! enctype "multipart/form-data" ! A.method "POST" ! action "/upload" $ do
-- >                input ! type_ "file" ! name "file_upload" ! size "40"
-- >                input ! type_ "submit" ! value "upload"
-- >
-- >     handleUpload :: ServerPart Response
-- >     handleUpload =
-- >         do (tmpFile, uploadName, contentType) <- lookFile "file_upload"
-- >            ok $ template "file uploaded" $ do
-- >                 p (toHtml $ "temporary file: " ++ tmpFile)
-- >                 p (toHtml $ "uploaded name:  " ++ uploadName)
-- >                 p (toHtml $ "content-type:   " ++ show contentType)
