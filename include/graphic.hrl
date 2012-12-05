-include_lib("nitrogen_core/include/wf.hrl").
-record(graphic, {
    ?ELEMENT_BASE(element_graphic),
    ratio = 1.4,
    data = [],
    chartType = stock :: stock|chart,
    type = line :: undefined|line|spline|area|areaspline|column|bar|pie|scatter,
    range_selector = false,
    scrollbar = false,
    navigator = false,
    ordinal = false,
    client_id
  }).

