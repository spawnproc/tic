Node.prototype.on = Node.prototype.addEventListener
var $ = document.querySelector.bind(document)

instrument.on('click', e=>markets.classList.add('show'))

let instruments = new Map()
for (let a of document.querySelectorAll('.market-instrument')) {
    instruments.set(a, a.parentElement.firstElementChild.textContent + ' ' + a.textContent)
}

let selectedInstrument = $('.market-instrument')
selectedInstrument.classList.add('selected-instrument')

markets.on('click', function(e) {
    if (instruments.has(e.target)) {
        instrument.textContent = instruments.get(e.target)
        markets.classList.remove('show')
        if (selectedInstrument !== e.target) {
            console.log(e.target)
            selectedInstrument.classList.remove('selected-instrument')
            selectedInstrument = e.target
            selectedInstrument.classList.add('selected-instrument')
        }
    }
})

let selectedWidget = $('.mobile-title')
selectedWidget.classList.add('selected-widget')
window[selectedWidget.textContent].classList.add('show')
window[selectedWidget.textContent].classList.add('show')

for (let w of document.querySelectorAll('.mobile-title')) {
    w.on('click', showWidget)
}

function showWidget(e) {
    if (this !== selectedWidget) {
        selectedWidget.classList.remove('selected-widget')
        window[selectedWidget.textContent].classList.remove('show')
        window[selectedWidget.textContent].classList.remove('show')
        selectedWidget = e.target
        selectedWidget.classList.add('selected-widget')
        window[selectedWidget.textContent].classList.add('show')
        window[selectedWidget.textContent].classList.add('show')
    }
}

let tradesData = `13:11:17.789 +5257 1182.03 1.05
13:11:17.825 -5258 1188.49 1.10
13:11:17.833 +5259 1188.49 1.05
13:11:17.842 +5260 1188.49 1.03
13:11:17.919 -5261 1191.99 0.39
13:11:18.549 -5270 1188.49 0.97
13:11:18.550 +5271 1188.39 0.87
13:11:18.562 +5272 1188.39 0.98
13:11:18.568 +5273 1188.39 1.05
13:11:18.581 -5274 1188.39 1.09
13:11:17.789 +5257 1182.03 1.05
13:11:17.825 -5258 1188.49 1.10
13:11:17.833 +5259 1188.49 1.05
13:11:17.842 +5260 1188.49 1.03
13:11:17.919 -5261 1191.99 0.39
13:11:18.549 -5270 1188.49 0.97
13:11:18.550 +5271 1188.39 0.87
13:11:18.562 +5272 1188.39 0.98
13:11:18.568 +5273 1188.39 1.05
13:11:18.581 -5274 1188.39 1.09
13:11:17.789 +5257 1182.03 1.05
13:11:17.825 -5258 1188.49 1.10
13:11:17.833 +5259 1188.49 1.05
13:11:17.842 +5260 1188.49 1.03
13:11:17.919 -5261 1191.99 0.39
13:11:18.549 -5270 1188.49 0.97
13:11:18.550 +5271 1188.39 0.87
13:11:18.568 +5273 1188.39 1.05
13:11:18.581 -5274 1188.39 1.09
13:11:17.789 +5257 1182.03 1.05
13:11:17.825 -5258 1188.49 1.10
13:11:17.833 +5259 1188.49 1.05
13:11:17.842 +5260 1188.49 1.03
13:11:17.919 -5261 1191.99 0.39
13:11:18.549 -5270 1188.49 0.97
13:11:18.550 +5271 1188.39 0.87
13:11:18.562 +5272 1188.39 0.98
13:11:18.568 +5273 1188.39 1.05
13:11:18.581 -5274 1188.39 1.09`.split('\n').map(i=>i.split(' '))

tradesData.forEach(([time,seq,price,size])=>{
    tradesTable.insertAdjacentHTML('afterbegin', `<div><span class="${seq[0] == '-' ? 'ask' : 'bid'}">${price}</span><span>${size}</span><span>${time.substring(0, 11)}</span></div>`)
}
)

tradesData.forEach(([time,seq,price,size])=>{
    ordersTable.insertAdjacentHTML('afterbegin', `<div><span>${size}</span><span class="ask">${price}</span><span class="bid">${price}</span><span>${size}</span></div>`)
}
)
