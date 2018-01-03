module Update exposing (update)

import Http exposing (Error(..))
import Json.Decode as Decode
import Decoders exposing (validationErrorsDecoder)

import Commands as Commands
import Messages exposing (Msg(..))
import Model exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    subscribeForm =
      model.subscribeForm

    formFields =
      extractFormFields model.subscribeForm

  in
    case msg of
        HandleFullNameInput value ->
            { model | subscribeForm = Editing { formFields | fullName = value }} ! []
        
        HandleEmailInput value ->
          { model | subscribeForm = Editing { formFields | email = value }} ! []
        
        HandleFormSubmit ->
          let
              newSubscribeForm =
                Saving formFields
          in
            { model | subscribeForm = Saving formFields } ! [ Commands.subscribe newSubscribeForm]
        
        SubscribeResponse (Ok result) ->
          { model | subscribeForm = Success} ! []
        
        SubscribeResponse (Err (BadStatus response)) ->
          case Decode.decodeString validationErrorsDecoder response.body of
            Ok validationErrors ->
              { model | subscribeForm = Invalid formFields validationErrors} ! []
            
            Err error ->
              { model | subscribeForm = Errored formFields "Shit, something went down - my bad."} ! []
        
        SubscribeResponse (Err error) ->
          { model | subscribeForm = Errored formFields "Shit, something went down - my bad."} ! []