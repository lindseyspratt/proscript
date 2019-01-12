function predicate_dom_element_add_event_listener(element, event, goal) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(event) !== TAG_ATM) {
        instantiation_error(event);
    }

    if (TAG(goal) !== TAG_STR && TAG(goal) !== TAG_ATM) {
        instantiation_error(goal);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var eventJS = PL_atom_chars(event);

    var goalJS = format_term(goal, {quoted:true});

    function handlerFunction () {
        proscript(goalJS)
    }

    elementJS.addEventListener(eventJS, handlerFunction);

    return true;
}

function predicate_dom_element_blur(element) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    elementJS.blur();

    return true;
}

function predicate_dom_element_focus(element) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    elementJS.focus();

    return true;
}

function predicate_dom_element_click(element) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    elementJS.click();

    return true;
}

function predicate_dom_element_clone_node(element, flag, clone) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(flag) !== TAG_ATM) {
        instantiation_error(flag);
    }

    if (TAG(clone) !== TAG_REF) {
        instantiation_error(clone);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var flagJS = PL_atom_chars(flag); //atable[VAL(flag)];
    if(flagJS !== "true" && flagJS !== "false") {
        domain_error('not_valid_boolean', flag);
    }

    var cloneElementJS = elementJS.cloneNode(flagJS === "true");

    var cloneElementPL = create_element_structure(cloneElementJS);
    return unify(clone, cloneElementPL);
}

function predicate_dom_element_compare_document_position(element, otherElement, comparison) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(otherElement) !== TAG_STR) {
        instantiation_error(otherElement);
    }

    if (TAG(comparison) !== TAG_REF) {
        instantiation_error(comparison);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var otherElementObject = {};
    if (!get_element_object(otherElement, otherElementObject)) {
        return false;
    }
    var otherElementJS = otherElementObject.value;

    var comparisonJS = elementJS.compareDocumentPosition(otherElementJS);
    var comparisonPL = PL_put_integer(comparisonJS);
    return unify(comparison, comparisonPL);
}

function predicate_dom_element_contains(element, otherElement) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(otherElement) !== TAG_STR) {
        instantiation_error(otherElement);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var otherElementObject = {};
    if (!get_element_object(otherElement, otherElementObject)) {
        return false;
    }
    var otherElementJS = otherElementObject.value;

    return elementJS.contains(otherElementJS);
}

function predicate_dom_element_bounding_client_rect(element, rect) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(rect) !== TAG_STR && TAG(rect) !== TAG_REF) {
        instantiation_error(rect);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var rectJS = elementJS.getBoundingClientRect();

    var ftor = lookup_functor('dom_rect', 8);
    var rectPL = alloc_structure(ftor);
    memory[state.H++] = PL_put_integer(rectJS.left);
    memory[state.H++] = PL_put_integer(rectJS.top);
    memory[state.H++] = PL_put_integer(rectJS.right);
    memory[state.H++] = PL_put_integer(rectJS.bottom);
    memory[state.H++] = PL_put_integer(rectJS.x);
    memory[state.H++] = PL_put_integer(rectJS.y);
    memory[state.H++] = PL_put_integer(rectJS.width);
    memory[state.H++] = PL_put_integer(rectJS.height);

    return unify(rect, rectPL);
}

function predicate_dom_element_insert_adjacent_element(element, mode, otherElement) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(otherElement) !== TAG_STR) {
        instantiation_error(otherElement);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var modeJS = PL_atom_chars(mode);
    if(["afterbegin", "afterend", "beforebegin", "beforeend"].indexOf(modeJS) === -1) {
        domain_error("not_valid_insert_adjacent_mode", mode);
    }

    var otherElementObject = {};
    if (!get_element_object(otherElement, otherElementObject)) {
        return false;
    }
    var otherElementJS = otherElementObject.value;

    elementJS.insertAdjacentElement(modeJS, otherElementJS);
    return true;
}

function predicate_dom_element_insert_adjacent_html(element, mode, html) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(html) !== TAG_LST) {
        instantiation_error(html);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var modeJS = PL_atom_chars(mode);
    if(["afterbegin", "afterend", "beforebegin", "beforeend"].indexOf(modeJS) === -1) {
        domain_error("not_valid_insert_adjacent_mode", mode);
    }

    var htmlJS = codes_to_string(html);

    elementJS.insertAdjacentHTML(modeJS, htmlJS);
    return true;
}

function predicate_dom_element_insert_adjacent_text(element, mode, text) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(html) !== TAG_LST) {
        instantiation_error(html);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var modeJS = PL_atom_chars(mode);
    if(["afterbegin", "afterend", "beforebegin", "beforeend"].indexOf(modeJS) === -1) {
        domain_error("not_valid_insert_adjacent_mode", mode);
    }

    var textJS = codes_to_string(text);

    elementJS.insertAdjacentText(modeJS, textJS);
    return true;
}

function predicate_dom_element_is_equal_node(element, otherElement) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(otherElement) !== TAG_STR) {
        instantiation_error(otherElement);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var otherElementObject = {};
    if (!get_element_object(otherElement, otherElementObject)) {
        return false;
    }
    var otherElementJS = otherElementObject.value;

    return elementJS.isEqualNode(otherElementJS);
}

function predicate_dom_element_normalize(element) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    elementJS.normalize();
    return true;
}

function predicate_dom_element_scroll_into_view(element, flag) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(flag) !== TAG_ATM) {
        instantiation_error(flag);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var flagJS = PL_atom_chars(flag);
    if(flagJS !== "true" && flagJS !== "false") {
        domain_error('not_valid_boolean', flag);
    }

    elementJS.scrollIntoView(flagJS === "true");
    return true;
}