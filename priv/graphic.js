(function() {
  function tooltipFormatter() {
    // Always print smal timestamp
    var t = '<span style="font-size:xx-small">' + Highcharts.dateFormat('%Y-%m-%d %H:%M:%S', this.x) + '</span><br/>';
    $.each(this.points, function(i, item) {
      var point = item.point, series = item.series;
      var pointDesc = describePoint(point);
      // Print description for each selected (series, point) pair
      t += '<br><span style="color:'+series.color+'">'+series.name+'</span>: <b>'+pointDesc+'</b>';
    });
    return t;
  };

  function describePoint(point) {
    // simple data
    if (isNaN(point.close)) return point.y.toFixed(3);
    else { // OHLC or candlestick
      return "[" + point.open.toFixed(3) + ", " + point.high.toFixed(3) + ", " + point.low.toFixed(3) + ", " + point.close.toFixed(3) + "]"
    }
  };

  function renderGraphic(ID, Options, Data) {
    // Destroy existing chart with same ID if any
    var chart = window.graphics[ID];
    if (chart) chart.destroy();

    // Reset zero options to empty object (for cases when we get null, undefined, [] or so)
    if (! Options) Options = {};

    // select between Chart and StockChart
    var chartType = Options.chartType || "StockChart";

    // Display options
    var chartOptions = {renderTo: ID, animation: false};
    // Default graph type -- line, ohlc, etc.
    if(Options.type) chartOptions.type = Options.type;

    // Set series ids for use with Chart.get(id)
    $.each(Data, function(i, s) {s.id = "series-" + s.name;});

    // ordinal = false makes point interval be proportional to actual time difference
    var xAxis = {ordinal: Options.ordinal};
    // categories for bar charts
    if(Options.categories) xAxis.categories = Options.categories;

    // Y axis is passed as-is
    var yAxis = Options.yAxis;

    // Make floating title if needed
    var title = undefined;
    if (Options.title) title = {text: Options.title, floating: true};

    // Navigator logic
    var navigator = {height:20}; // Narrow to save space
    if (typeof(Options.navigator) == "string" || typeof(Options.navigator) == "number") {
      // We have explicit series for navigator
      navigator.enabled = true;
      navigator.baseSeries = Options.navigator;
    } else
      navigator.enabled = Options.navigator || (maxDataLen(Data) > 500);

    // X Range options -- set dynamic or static range if requested
    if (Options.range == "dynamic") { // Magic range for lazy loading
      xAxis.events = {afterSetExtremes : afterSetExtremes}; // Install hook
      navigator.adaptToUpdatedData = false; // Avoid continious updates
    } else if (!isNaN(Options.range)) { // Numeric range
      xAxis.range = Options.range;
    }

    var legend = {enabled:false};
    if (Options.legend) {
      legend.enabled = true;
      legend.floating = true;
      if (Options.legend == true) {
        legend.align = "center";
        legend.y = (navigator.enabled)?-50:-20;
      } else legend.align = Options.legend;
    }

    var args = {
      chart: chartOptions,
      rangeSelector: {enabled: Options.range_selector || false},
      scrollbar: {enabled: Options.scrollbar || false},
      navigator: navigator,
      xAxis: xAxis,
      yAxis: yAxis,
      title: title,
      legend: legend,
      credits: {enabled: false},
      tooltip: {formatter: tooltipFormatter},
      series: Data,
      plotOptions: {ohlc: {grouping: false, lineWidth: 2}}
    };

    chart = new Highcharts[chartType](args);

    window.graphics[ID] = chart;
    return chart;
  };

  function requestWsGraphic(ID, MFA) {
    // Generate WS uri based on current host
    var uri = "ws://" + window.location.host + "/graphic";
    var s = new WebSocket(uri);

    // Send MFA jyst after open
    s.onopen = function(evt) {
      s.send(JSON.stringify({mfa: MFA}));
    };

    // Accept message
    s.onmessage = function(evt) {
      var data = JSON.parse(evt.data);
      if (data.init) {
        // Initial render
        var graph = renderGraphic(ID, data.options, data.data);
        // Store websocket for event handlers
        graph.websocket = s;
        // Anything user wants to attach to graphic
        graph.custom_data = data.custom;
        // To improve viewing speed we trigger redraw externally, not on every data packet
        startRedraw(graph, 250);
      } else if (data.set) {
        setGraphicData(ID, data);
      } else {
        updateGraphic(ID, data);
      };
    };
  };

  function updateGraphic(ID, Data) {
    var chart = window.graphics[ID];
    for (var s in Data) {
      var series = chart.get("series-" + s);
      updateSeries(series, Data[s]);
    };
  };

  function updateSeries(series, points) {
    var count = points.length;

    for (var i = 0; i < count; i++) {
      series.addPoint(points[i], false);
    };
  };

  function setGraphicData(ID, Data) {
    var chart = window.graphics[ID];
    for (var s in Data) {
      if (Data[s] == true) continue; // Flag
      var series = chart.get("series-" + s);
      series.setData(Data[s], false);
    };
    chart.redraw();
  };

  function autoHeight(elt, ratio) {
    $(elt).height(elt.clientWidth/ratio);
  };

  function maxDataLen(Data) {
    var max = 0;
    for (name in Data) { max = Math.max(max, Data[name].length) };
    return max;
  };

  function startRedraw(graph, interval) {
    setTimeout(function() {
      graph.redraw();
      startRedraw(graph, interval);
    }, interval);
  };

  function afterSetExtremes(e) {
    // Extract corresponding socket
    var chart = e.target.chart; if (!chart) return false;
    var s = chart.websocket; if (!s) return false;

    // Extract bounds
    var min = Math.round(e.min);
    var max = Math.round(e.max);

    // Send query
    s.send(JSON.stringify({type:"range", min:min, max:max}));
  };

  window.Graphic = {
    autoHeight: autoHeight,
    render: renderGraphic,
    ws_request: requestWsGraphic };
  window.graphics = {};
})();
