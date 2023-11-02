require 'minitest/autorun'
require_relative '../lib/Card.rb'
require_relative '../lib/Deck.rb'


class SuitCountTest < Minitest::Test
  def setup
    @deck = Deck.new([Card.new(Card.suits[0], 1)])
  end

  def test_suit_count
    card1 = Card.new(Card.suits[0], 1)
    card2 = Card.new(Card.suits[0], 2)
    card3 = Card.new(Card.suits[1], 3)
    @deck.instance_variable_set(:@cards, [card1, card2, card3])
    assert_equal 2, @deck.suitCount(Card.suits[0])
    assert_equal 1, @deck.suitCount(Card.suits[1])
    assert_equal 0, @deck.suitCount(Card.suits[2])
  end
end