class Card
    @@suits = ['Copa', 'Espada','Oro', 'Basto']
    @@ranks = (1..12)
    attr_reader :suit, :rank
    def initialize(suit, rank)
        if !(@@suits.include?suit)
            raise "Palo no reconocido"
        end
        @suit = suit
        
        if !(@@ranks === rank)
            raise "Numero fuera de rango"
        end
        @rank = rank
    end
    def self.suits
        @@suits
    end
    def self.ranks
        @@ranks
    end
end  