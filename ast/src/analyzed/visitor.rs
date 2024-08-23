use crate::parsed::visitor::VisitOrder;

use self::parsed::visitor::AllChildren;

use super::*;

impl<T> ExpressionVisitable<AlgebraicExpression<T>> for AlgebraicExpression<T> {
    fn visit_expressions_mut<F, B>(&mut self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&mut AlgebraicExpression<T>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        self.children_mut()
            .try_for_each(|e| e.visit_expressions_mut(f, o))?;
        if o == VisitOrder::Post {
            f(self)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_expressions<F, B>(&self, f: &mut F, o: VisitOrder) -> ControlFlow<B>
    where
        F: FnMut(&AlgebraicExpression<T>) -> ControlFlow<B>,
    {
        if o == VisitOrder::Pre {
            f(self)?;
        }
        self.children()
            .try_for_each(|e| e.visit_expressions(f, o))?;
        if o == VisitOrder::Post {
            f(self)?;
        }
        ControlFlow::Continue(())
    }
}

impl<T> AllChildren<AlgebraicExpression<T>> for AlgebraicExpression<T> {
    fn all_children(&self) -> Box<dyn Iterator<Item = &AlgebraicExpression<T>> + '_> {
        Box::new(iter::once(self).chain(self.children().flat_map(|e| e.all_children())))
    }

    fn all_children_mut(&mut self) -> Box<dyn Iterator<Item = &mut AlgebraicExpression<T>> + '_> {
        let self_iter = iter::once(self as *mut _);
        let children_iter = self
            .children_mut()
            .flat_map(|child| child.all_children_mut());
        let self_iter = self_iter.map(|ptr| unsafe { &mut *ptr });
        Box::new(self_iter.chain(children_iter))
    }
}
