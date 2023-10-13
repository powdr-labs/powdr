
#pragma once
#include "../relation_parameters.hpp"
#include "../relation_types.hpp"

namespace proof_system::example_vm {

template <typename FF> class exampleImpl {
    public:
        
        static constexpr size_t RELATION_LENGTH = 3;
   static constexpr size_t DEGREE_0 = 3;
   static constexpr size_t DEGREE_1 = 3;
   static constexpr size_t DEGREE_2 = 3;
   static constexpr size_t DEGREE_3 = 3;
template <template <size_t...> typename SubrelationAccumulatorsTemplate>
    using GetAccumulatorTypes = SubrelationAccumulatorsTemplate<DEGREE_0, DEGREE_1, DEGREE_2, DEGREE_3>;
        
        
    template <typename AccumulatorTypes>
    void static accumulate(
        typename AccumulatorTypes::Accumulators& evals,
        const auto& new_term,
        [[maybe_unused]] const RelationParameters<FF>&,
        [[maybe_unused]] const FF& scaling_factor
    ){

    //Contribution 0
    {

        using View = typename std::tuple_element<0, typename AccumulatorTypes::AccumulatorViews>::type;
       auto Fibonacci_ISLAST = View(new_term.Fibonacci_ISLAST);
   auto Fibonacci_x = View(new_term.Fibonacci_x);
   auto Fibonacci_y = View(new_term.Fibonacci_y);
   auto Fibonacci_x_shift = View(new_term.Fibonacci_x_shift);
   auto Fibonacci_y_shift = View(new_term.Fibonacci_y_shift);
    
    auto tmp = (Fibonacci_ISLAST * (Fibonacci_y_shift - FF(1)));
    tmp *= scaling_factor;
    std::get<0>(evals) += tmp;
}
//Contribution 1
    {

        using View = typename std::tuple_element<1, typename AccumulatorTypes::AccumulatorViews>::type;
       auto Fibonacci_ISLAST = View(new_term.Fibonacci_ISLAST);
   auto Fibonacci_x = View(new_term.Fibonacci_x);
   auto Fibonacci_y = View(new_term.Fibonacci_y);
   auto Fibonacci_x_shift = View(new_term.Fibonacci_x_shift);
   auto Fibonacci_y_shift = View(new_term.Fibonacci_y_shift);
    
    auto tmp = (Fibonacci_ISLAST * (Fibonacci_x_shift - FF(1)));
    tmp *= scaling_factor;
    std::get<1>(evals) += tmp;
}
//Contribution 2
    {

        using View = typename std::tuple_element<2, typename AccumulatorTypes::AccumulatorViews>::type;
       auto Fibonacci_ISLAST = View(new_term.Fibonacci_ISLAST);
   auto Fibonacci_x = View(new_term.Fibonacci_x);
   auto Fibonacci_y = View(new_term.Fibonacci_y);
   auto Fibonacci_x_shift = View(new_term.Fibonacci_x_shift);
   auto Fibonacci_y_shift = View(new_term.Fibonacci_y_shift);
    
    auto tmp = ((FF(1) - Fibonacci_ISLAST) * (Fibonacci_x_shift - Fibonacci_y));
    tmp *= scaling_factor;
    std::get<2>(evals) += tmp;
}
//Contribution 3
    {

        using View = typename std::tuple_element<3, typename AccumulatorTypes::AccumulatorViews>::type;
       auto Fibonacci_ISLAST = View(new_term.Fibonacci_ISLAST);
   auto Fibonacci_x = View(new_term.Fibonacci_x);
   auto Fibonacci_y = View(new_term.Fibonacci_y);
   auto Fibonacci_x_shift = View(new_term.Fibonacci_x_shift);
   auto Fibonacci_y_shift = View(new_term.Fibonacci_y_shift);
    
    auto tmp = ((FF(1) - Fibonacci_ISLAST) * (Fibonacci_y_shift - (Fibonacci_x + Fibonacci_y)));
    tmp *= scaling_factor;
    std::get<3>(evals) += tmp;
}
}

};

template <typename FF> using example = exampleImpl<FF>;

        }