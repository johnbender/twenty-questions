(function( window, $ ) {
  var editor = window.ace.edit( "editor" );
  editor.setTheme( "ace/theme/solarized_light" );
  editor.getSession().setMode( "ace/mode/haskell" );
  editor.setKeyboardHandler( "ace/keyboard/emacs" );
})( this, jQuery );
