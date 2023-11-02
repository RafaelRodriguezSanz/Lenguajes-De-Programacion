require_relative 'expressions'

class ExpressionFactory
  @@instance = ExpressionFactory.new

  def self.instance
    @@instance
  end

  private_class_method :new

  @@numeral_instances = {}
  @@truth_value_instances = {}

  (0..10).each do |i|
    @@numeral_instances[i] = Numeral.new(i)
  end

  [true, false].each do |bool|
    @@truth_value_instances[bool] = TruthValue.new(bool)
  end

  def create(value)
    if @@numeral_instances.include?(value)
      @@numeral_instances[value] ||= Numeral.new(value)
    elsif @@truth_value_instances.include?(value)
      @@truth_value_instances[value] ||= TruthValue.new(value)
    else
      raise "Tipo no reconocido"
    end
  end
end
