module View exposing (view)


import Html exposing (Html, form)
import Html.Attributes as Html
import Html.Events as Html
import Dict exposing (Dict)

import Messages exposing (Msg(..))
import Model exposing (..)

view : Model -> Html Msg
view { subscribeForm } =
  case subscribeForm of
    Success ->
      successView
    
    _ ->
      formView subscribeForm

successView : Html Msg
successView =
  Html.div 
    [ Html.class "success-message"]
    [ Html.div
      [ Html.class "icon is-large"]
      [ Html.i
        [ Html.class "fa fa-3 fa-heart" ]
        []
      ]
    , Html.h2
      []
      [ Html.text "Congrats ~  you have successfully subscribed" ]
    , Html.p
      []
      [ Html.text "Will keep you updated with the lastest news!"]
    ]

formView : SubscribeForm -> Html Msg
formView subscribeForm =
  let
    validationErrors =
      extractValidationErrors subscribeForm

    { fullName, email } =
      extractFormFields subscribeForm
    
    saving =
      case subscribeForm of
        Saving _ ->
          True

        _ ->
          False
    
    invalid =
      case subscribeForm of
        Invalid _ _ ->
          True
        
        _ -> 
          False
    
    buttonDisabled =
      fullName == "" || email == "" || saving || invalid
  in
      Html.div
        [ Html.class "content"]
        [ Html.h3
          []
          [ Html.text "Stay in Contact"]
        , Html.p
          []
          [ Html.text "Subscribe to stay updated"]
        , formError subscribeForm
        , form
          [ Html.onSubmit HandleFormSubmit]
          [ Html.div
            [ Html.class "field"]
            [ Html.div
              [ Html.class "control"]
              [ Html.input
                [ Html.classList
                  [ ( "input is-medium", True)
                  , ("is-danger", Dict.member "full_name" validationErrors)
                  ]
                  , Html.type_ "text"
                  , Html.placeholder "Full Name"
                  , Html.required True
                  , Html.value fullName
                  , Html.onInput HandleFullNameInput
                ]
                []
                , validationErrorView "full_name" validationErrors
              ]
            ]
          , Html.div
            [ Html.class "field"]
            [ Html.div
              [ Html.class "control"]
              [ Html.input
                [ Html.classList
                  [ ( "input is-medium", True)
                  , ( "is-danger", Dict.member "email" validationErrors)
                  ]
                  , Html.type_ "email"
                  , Html.placeholder "Email"
                  , Html.required True
                  , Html.value email
                  , Html.onInput HandleEmailInput
                ]
                []
              ]
            ]
          , Html.div
            [ Html.class "field"]
            [ Html.div
              [ Html.class "control"]
              [ Html.button
                [ Html.classList
                  [ ("button is-primary is-medium", not saving)
                  , ("button is-primary is-loading is-medium", saving)
                  ]
                , Html.disabled buttonDisabled
                ]
                [ Html.span
                  []
                  [ Html.text "subscribe Me"]
                ]
              ]
            ]
          ]
        ]

formError : SubscribeForm -> Html Msg
formError subscribeForm =
  case subscribeForm of
    Errored _ message ->
      Html.div
        [ Html.class "notfication is-danger fade-in"]
        [ Html.text message]
    
    _ ->
      Html.text ""

validationErrorView : String -> ValidationErrors -> Html Msg
validationErrorView key validationErrors =
  case Dict.get key validationErrors of
    Just error ->
      error
        |> List.map Html.text
        |> Html.p
          [ Html.class "help is-danger"]
    
    Nothing ->
      Html.text ""