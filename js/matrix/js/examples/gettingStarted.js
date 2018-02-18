function gettingStarted(n) {
    switch (n) {
        case 0:
            // we can offer one node....
            return h1( "Hello, world. (single node) case=" +n);
            break;
        case 1:
            // ...or an array of nodes.
            return [ h1("Hello, world. (multi node) case=" +n),
                    h2("Booya!")];
            break;
        case 2:
             // If a map is the first argument, we take it to be HTML attributes
            // return [h2( {style: "background-color:yellow"}, ["Credits RSN....."])];
            return [ section({ class: "todoapp"},
                        p("hi mom") //header({class: "header"})
            ),

                h2( {style: "background-color:yellow"}, ["Credits RSN..."])];
            break;

        case 3: // Web components? We don't need no stinkin web components!
            return [ section({ class: "todoapp"},
                        header({class: "header"},
                            h1("todos"))),

                // any function with any parameters is fine, just return
                // a DOM-generating function....
                todoMVCCredits('<a href="http://tiltontec.com">Kenny Tilton')];
            break;

        case 4: // JSX? Pre-processor? We don't need no etc!
            return [ section({ class: "todoapp"},
                header({class: "header"},
                    h1("todos"))),

                // see this function for why we call it 'without JSX'
                todoMVCCreditsSansJSX('<a href="http://tiltontec.com">Kenny Tilton')];
            break;

        case 5:
            return tag("h1",
                    {style: "background-color:yellow"},
                    "Just attrs (bkg yellow) and string content");
            break;

        case 6:
            return tag("h1",
                    {style: "background-color:yellow"},
                    {test42: cF( c => 40+2)},
                    cF( c=> "just attrs"));
            break;
        default:
            return h1("Undefined gettingStarted case: " +n);
    }
}

function todoMVCCredits (createdBy) {
    // simulated "web component", aka custom Tag
    return footer({class: "info"},
            p('Double-click the text of a todo to change it'),
            p('Created by '+createdBy),
            p('Inspired by <a href="http://todomvc.com">TodoMVC</a>'));
}

function todoMVCCreditsSansJSX (createdBy) {
    // credits showing off full power of JS for page building -- without JSX
    return footer({class: "info"},
        ['Double-click the text of a todo to change it',
            'Created by '+createdBy,
            'Inspired by <a href="http://todomvc.com">TodoMVC</a>']
            .map( s => p({},s)));
}
window['gettingStarted'] = gettingStarted;