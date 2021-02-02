# exceptiont

This package provides a monad transformer that implements `MonadError`
using IO exceptions. In particular, this is useful when you want your
stack to be an instance of both `MonadError` and `MonadUnliftIO`, which
you can't do with `ExceptT`.
