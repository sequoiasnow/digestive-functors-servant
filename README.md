# digestive-functors-servant

A library allowing the use of
[digestive-functors](http://hackage.haskell.org/package/digestive-functors) for
[servant](http://hackage.haskell.org/package/servant) based
applications. Because the point of servant is to ensure statically typed API's
the standard `runForm` approach is rather underwhelming. Instead we introduce a
new type `Formic` which wraps around the `runForm` behavior to allow the
validation of the form to occur during the route matching phase.

## XSRF

In order to prevent cross site forgery requests, we also set an `XSRF` token in
both the form, and form view in order to verify the request.

## Example

Let's consider a standard login form. We wish to capture some login information,
such as:

```haskell
data Login = Login { loginName :: Text, loginPassword :: Text }
```

We can capture this input as a form using digestive-functors.

```haskell
loginForm :: Form Text Handler Login
loginForm = Login
    <$> "login" .: text Nothing
    <*> "password" .: text Nothing
```

We can also render our form as a view. In order to protect from XSRF attacks,
we will include a hidden input with the XSRF token

```haskell
loginView :: Html () -> View Text -> Html ()
loginView hiddenXSRFInput view = do
    form_ [method_ "post"] $ do
        toHtmlRaw hiddenXSRFInput

        input "login" view "Login"
        password "password" view "Password"
```

This in hand we can now define our login API. Following the tutorial in
[servant-auth](http://github.com/haskell-servant/servant-auth#readme) this might
look something like:

```haskell
type LoginAPI =
  "login" :> FormicRoute HTML (Html ()) "loginForm" [LoginErr] App (Entity User)
    (Post '[HTML] (Headers '[ Header "Set-Cookie" SetCookie
                              , Header "Set-Cookie" SetCookie ]
                      (Html ())))
```

If we expand what's going on here, we see that we are accepting a `Get` request
which renders our view, and post request which handles parsing our form.

```haskell
type LoginAPI =
    "login" :> (
        Get '[HTML] (Headers '[ Header "Set-Cookie" SetCookie ] (Html ()) :<|>
        ReqBody '[FormUrlEncoded] (Formic "loginForm" Text Handler Login)
            :> ReqCookies '[ReqCookie "XSRF-TOKEN" XSRFToken] :
            :> (Post '[HTML] (Headers '[ Header "Set-Cookie" SetCookie
                                       , Header "Set-Cookie" SetCookie ]
                      (Html ())))
```
```

Under the hood, we can see that what we are actually parsing is a `Formic` which
runs our form and gets the `Login` result before passing it to our handler. We
can use `formicServer` to implement this route

```haskell
loginHandler :: ServerT LoginAPI Handler
loginHandler = formicServer loginForm (pure . loginView) $ \ (Login username _)
->
    return $ h1_ $ "Hello " <> toHtml username <> "!"

```

### TODO

This library was built with Lucid in mind, and the extraction of that dependency
has created a number overly cumbersome type parameters. It might be best to
single out the application for blaze and lucid, or refactor these slightly to
avoid ambiguity.
