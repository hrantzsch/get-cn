# get-cn

A tool to extract text samples from news.sina.com.cn articles.

## Setup

- Install `haskell-stack`
- Clone the repository
- Run `stack setup` do install the dependencies
- Run `stack build` to build
- Run `stack exec get-cn-exe` to run

## Notes

- How to add new characters to split lines at?
  - The responsible function is `splitParagraph` in `Article.hs`. You can add new characters to the `seperators` list.
