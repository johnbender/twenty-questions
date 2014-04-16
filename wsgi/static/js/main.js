(function( window, $ ) {
  $(function() {
    var editor = window.ace.edit( "editor" );
    editor.setTheme( "ace/theme/solarized_light" );
    editor.getSession().setMode( "ace/mode/haskell" );
    editor.setKeyboardHandler( "ace/keyboard/emacs" );

    var cards = [
      {
        results: [
          "foo"
        ],
        names: [
          "bar"
        ]
      }
    ];

    var cardStack = new window.CardStack( cards, $("#card-template").text() );

    debugger;
    $( ".card-stack" ).html( cardStack.render() );
  });
})( this, jQuery );
