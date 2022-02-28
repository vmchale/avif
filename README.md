# avif

Make sure it passes its test suite. This will tell you whether the system
libavif is installed properly.

Also you might need to add

```cabal
extra-libraries: jpeg
```

to your `.cabal` file; I think something is wrong with libyuv.
