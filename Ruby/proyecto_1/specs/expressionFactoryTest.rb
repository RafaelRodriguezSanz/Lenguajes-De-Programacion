require 'minitest/autorun'
require_relative '../lib/expressionFactory'
require_relative '../lib/expressions'

class ExpressionFactoryTest < Minitest::Test
  def setup
    @factory = ExpressionFactory.instance
  end

  def test_create_numeral
    numeral1 = @factory.create(5)
    numeral2 = @factory.create(5)
    
    assert_instance_of Numeral, numeral1
    assert_equal 5, numeral1.value

    assert_same numeral1, numeral2
  end

  def test_create_truth_value
    truth_value1 = @factory.create(true)
    truth_value2 = @factory.create(true)

    assert_instance_of TruthValue, truth_value1
    assert_equal true, truth_value1.value

    assert_same truth_value1, truth_value2
  end

  def test_create_invalid_type
    assert_raises(RuntimeError) { @factory.create('hello') }
  end
end