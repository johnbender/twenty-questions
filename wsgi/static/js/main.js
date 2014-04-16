(function( window, $ ) {
  $(function() {
    var editor = window.ace.edit( "editor" );
    editor.setTheme( "ace/theme/solarized_light" );
    editor.getSession().setMode( "ace/mode/haskell" );
    editor.setKeyboardHandler( "ace/keyboard/emacs" );

    var cards = [
      {
        left: {
          results: [
            "foo"
          ],
          names: [
            "bar"
          ]
        },
        right: {
          results: [
            "baz"
          ],
          names: [
            "bak"
          ]
        }
      },
      {
        left: {
          results: [
            "foo"
          ],
          names: [
            "bar"
          ]
        },
        right: {
          results: [
            "baz"
          ],
          names: [
            "bak"
          ]
        }
      }
    ];

    var cardStack = new window.CardStack( cards, $("#card-template").text() );

    $( ".card-stack" ).html( cardStack.render() );
  });
})( this, jQuery );
