context('Test Hacn', () => {
  beforeEach(() => {
    cy.visit('http://localhost:8080')
  })

  describe('Implicit Assertions', () => {
    it('blah', () => {
      cy.get('[data-testid="test"]')
        .should('have.text', 'Say hello!')
    });
  });
});
