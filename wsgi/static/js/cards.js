(function( window, $ ) {
  var CardStack = window.CardStack = function( cards, template ) {
    this.cards = cards;
    this.template = template;
  };

  CardStack.prototype.render = function() {
    return $.map(this.cards, $.proxy(function( card ) {
      return window.Mustache.render( this.template, card );
    }, this)).join( " " );
  };
})( this, jQuery );
