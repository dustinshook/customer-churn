/* ----- REACTABLE CELL RENDERER FUNCTIONS ----- */

function paymentBadge(cellInfo) {
    return `<div class="payment-col">${cellInfo.value}<span class="invoice-paid">Success</span></div>`;
}