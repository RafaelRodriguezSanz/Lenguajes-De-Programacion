require_relative 'Card'
class Deck
    @@full
  attr_reader :cards
  def initialize(cards)
    if cards.respond_to?("each")
      aux = true
      cards.each do |card|
        aux = aux && (card.is_a?Card)
      end
    end
    if aux
      @cards = cards
    else
      raise "No es un array de cartas"
    end
  end
  
  def shuffle()
    @cards.shuffle!
        return self
    end
  def deal(n)
    return Deck(@cards.shift(n))
  end

  def self.full
      full_deck = []
      Card.suits.each do |suit|
          Card.ranks.each do |rank|
              full_deck << Card.new(suit,rank)
          end
      end
      @@full = Deck.new(full_deck)
  end
  def peep()
    if @cards.empty?
        return nil
      else
        return @cards.first
      end
  end

  def suitCount(suit)
    if !(suit.include?suit)
        raise "Palo no reconocido"
    end
    count = 0
    @cards.each do |card|
        if card.suit == suit
          count = count + 1
        end
    end 
    count        
  end

end