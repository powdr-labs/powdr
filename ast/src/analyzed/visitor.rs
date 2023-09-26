use super::*;

impl<T> ExpressionVisitor<T, Reference> for Analyzed<T> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        // TODO add constants if we change them to expressions at some point.
        self.definitions
            .values_mut()
            .try_for_each(|(_poly, definition)| match definition {
                Some(FunctionValueDefinition::Mapping(e))
                | Some(FunctionValueDefinition::Query(e)) => e.pre_visit_expressions_return_mut(f),
                Some(FunctionValueDefinition::Array(elements)) => elements
                    .iter_mut()
                    .flat_map(|e| e.pattern.iter_mut())
                    .try_for_each(|e| e.pre_visit_expressions_return_mut(f)),
                Some(FunctionValueDefinition::Expression(e)) => {
                    e.pre_visit_expressions_return_mut(f)
                }
                None => ControlFlow::Continue(()),
            })?;

        self.identities
            .iter_mut()
            .try_for_each(|i| i.pre_visit_expressions_return_mut(f))
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        // TODO add constants if we change them to expressions at some point.
        self.definitions
            .values()
            .try_for_each(|(_poly, definition)| match definition {
                Some(FunctionValueDefinition::Mapping(e))
                | Some(FunctionValueDefinition::Query(e)) => e.pre_visit_expressions_return(f),
                Some(FunctionValueDefinition::Array(elements)) => elements
                    .iter()
                    .flat_map(|e| e.pattern.iter())
                    .try_for_each(|e| e.pre_visit_expressions_return(f)),
                Some(FunctionValueDefinition::Expression(e)) => e.pre_visit_expressions_return(f),
                None => ControlFlow::Continue(()),
            })?;

        self.identities
            .iter()
            .try_for_each(|i| i.pre_visit_expressions_return(f))
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        // TODO add constants if we change them to expressions at some point.
        self.definitions
            .values_mut()
            .try_for_each(|(_poly, definition)| match definition {
                Some(FunctionValueDefinition::Mapping(e))
                | Some(FunctionValueDefinition::Query(e)) => e.post_visit_expressions_return_mut(f),
                Some(FunctionValueDefinition::Array(elements)) => elements
                    .iter_mut()
                    .flat_map(|e| e.pattern.iter_mut())
                    .try_for_each(|e| e.post_visit_expressions_return_mut(f)),
                Some(FunctionValueDefinition::Expression(e)) => {
                    e.post_visit_expressions_return_mut(f)
                }
                None => ControlFlow::Continue(()),
            })?;

        self.identities
            .iter_mut()
            .try_for_each(|i| i.post_visit_expressions_return_mut(f))
    }
}

impl<T> ExpressionVisitor<T, Reference> for Identity<T> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        self.left
            .selector
            .as_mut()
            .into_iter()
            .chain(self.left.expressions.iter_mut())
            .chain(self.right.selector.as_mut())
            .chain(self.right.expressions.iter_mut())
            .try_for_each(move |item| item.pre_visit_expressions_return_mut(f))
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        self.left
            .selector
            .as_ref()
            .into_iter()
            .chain(self.left.expressions.iter())
            .chain(self.right.selector.iter())
            .chain(self.right.expressions.iter())
            .try_for_each(move |item| item.pre_visit_expressions_return(f))
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        self.left
            .selector
            .as_mut()
            .into_iter()
            .chain(self.left.expressions.iter_mut())
            .chain(self.right.selector.as_mut())
            .chain(self.right.expressions.iter_mut())
            .try_for_each(move |item| item.post_visit_expressions_return_mut(f))
    }
}

impl<T> ExpressionVisitor<T, Reference> for SelectedExpressions<T> {
    fn pre_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        self.selector
            .as_mut()
            .into_iter()
            .chain(self.expressions.iter_mut())
            .try_for_each(move |item| item.pre_visit_expressions_return_mut(f))
    }

    fn pre_visit_expressions_return<F, B>(&self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        self.selector
            .as_ref()
            .into_iter()
            .chain(self.expressions.iter())
            .try_for_each(move |item| item.pre_visit_expressions_return(f))
    }

    fn post_visit_expressions_return_mut<F, B>(&mut self, f: &mut F) -> ControlFlow<B>
    where
        F: FnMut(&mut parsed::Expression<T, Reference>) -> ControlFlow<B>,
    {
        self.selector
            .as_mut()
            .into_iter()
            .chain(self.expressions.iter_mut())
            .try_for_each(move |item| item.post_visit_expressions_return_mut(f))
    }
}
