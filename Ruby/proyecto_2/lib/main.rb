require_relative 'Card'
require_relative 'Deck'

puts "Welcome to CardGame!"

card1 = [Card.new(Card.suits[0], 1), Card.new(Card.suits[0], 1)]
deck = Deck.new(card1)
puts deck.peep()
puts deck.suitCount(Card.suits[1])