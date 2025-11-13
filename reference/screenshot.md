# Save the current image in clipboard to png in your active directory

The screenshot will be saved as `.png` to a directory following these
rules

1.  In a regular RStudio project (or a Quarto book), it will be saved to
    a `images/` directory

2.  In a package project, it will be saved in a `man/figures` directory

3.  In a Quarto Blog project, it will save in the current post's folder.

4.  You can always override these defaults by setting `dir`

After using the shortcut Win + Shift + S, you can call this function!

## Usage

``` r
screenshot(file = NULL, proj = proj_get(), dir = NULL)
```

## Arguments

- file:

  A file name, ideally `-` (kebab-case). (extension ignored) (optional,
  default is `image.png`)

- proj:

  A project name

- dir:

  A directory (optional), to override the directory rules mentioned in
  the description. inside `proj`.

## Value

The full image path, invisibly.

## Details

If no file name is supplied, a file named `image0*.png` will be created.
The function then prompts you to rename the file with a more expressive
name. It will continue the numbering if a file named image exists.

Still have to validate if it works on macOS, as it is not clear whether
the image goes to the clipboard by default

The maximum number of images in a folder is 99. (only padding 2), should
be enough.

You should not be able to overwrite a screenshot with a generic name,
only a named one as it is possible you may require to retake your
screenshot.

## Examples

``` r
if (FALSE) {
  # Add an image to the clipboard
  # Run the following
  screenshot(file = "my-new-image")
}
```
