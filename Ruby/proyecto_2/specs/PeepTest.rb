require 'minitest/autorun'
require_relative '../lib/Card.rb'
require_relative '../lib/Deck.rb'

class PeepTest < Minitest::Test
  def setup
    @deck = Deck.new([Card.new(Card.suits[0], 1)])
  end

  def test_peep_empty_deck
    @deck = Deck.new([])
    assert_nil @deck.peep
  end

  def test_peep_with_cards
    card = Card.new(Card.suits[0], 1)
    @deck.instance_variable_set(:@cards, [card])
    assert_equal card, @deck.peep
  end
end
