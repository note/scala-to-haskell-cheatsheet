let T = ./types.dhall

let Prelude =
      https://prelude.dhall-lang.org/package.dhall sha256:4aa8581954f7734d09b7b21fddbf5d8df901a44b54b4ef26ea71db92de0b1a12

let menu =
      [ T.mkMenuItem T.SubPage.Basics "Basics" "index.html"
      , T.mkMenuItem T.SubPage.Adts "ADTs" "adts.html"
      , T.mkMenuItem T.SubPage.Lists "Lists" "lists.html"
      , T.mkMenuItem T.SubPage.Option "Option" "option.html"
      , T.mkMenuItem
          T.SubPage.ForComprehension
          "for comprehension"
          "for-comprehension.html"
      ]

let renderMenuItem =
          \(activeSubPage : T.SubPage)
      ->  \(menuItem : T.MenuItem)
      ->        if T.SubPage/equals menuItem.subPage activeSubPage

          then  ''
                <li><a href="${menuItem.filename}" class="selected">${menuItem.name}</a></li>
                ''

          else  ''
                <li><a href="${menuItem.filename}">${menuItem.name}</a></li>
                ''

let renderMenu =
          \(activeSubPage : T.SubPage)
      ->  Prelude.List.map T.MenuItem Text (renderMenuItem activeSubPage) menu

in  { renderMenu = renderMenu }
