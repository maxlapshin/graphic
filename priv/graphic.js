(function() {
  function tooltipFormatter() {
    // For charts (X is not time) prit pair of (x, y)
    if (this.point) return "(" + this.x + ", " + this.y + ")";

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
    var backend;
    if (Options.backend == "chart") backend = "Chart"
    else backend = "StockChart";

    // Display options
    var chartOptions = {renderTo: ID, animation: false};
    // Default graph type -- line, ohlc, etc.
    if(Options.type) chartOptions.type = Options.type;

    var dataLabels = {
      enabled:true,
      formatter:function() {return this.point.title;},
      y:-12,
      borderRadius:3,
      borderColor:"black",
      borderWidth:1,
      backgroundColor:"#bebebe"
    };

    // auto-add marks series
    var haveMarks = false;
    $.each(Data, function(i, s) {if (s.name == "$marks") haveMarks = true; });
    // if (!haveMarks) Data.push({name: "$marks", data: []});

    // Set series ids for use with Chart.get(id)
    $.each(Data, function(i, s) {
      s.id = "series-" + s.name;
      if (s.name == "$marks") {
        s.type = "scatter";
        s.color = "#40b040";
        s.dataLabels = dataLabels;
        setMarkLabels(s.data);
      };
    });

    // ordinal = false makes point interval be proportional to actual time difference
    var xAxis = {ordinal: Options.ordinal};
    // categories for bar charts
    if(Options.categories) xAxis.categories = Options.categories;

    // Y axis is passed as-is
    var yAxis = Options.yAxis;
    if (! yAxis) yAxis = {};

    if (Options.lines) yAxis.plotLines = genPlotLines(Options.lines);

    // Make floating title if needed
    var title = undefined;
    if (Options.title) title = {text: Options.title, floating: true};
    var subtitle = undefined;
    if (Options.subtitle) subtitle = {text: Options.subtitle, floating: true};

    // Navigator logic
    var navigator = {height:20}; // Narrow to save space
    if (typeof(Options.navigator) == "string") {
      navigator.enabled = true;
      navigator.baseSeries = "series-" + Options.navigator;
    } else if (typeof(Options.navigator) == "number") {
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
      subtitle: subtitle,
      legend: legend,
      credits: {enabled: false},
      tooltip: {formatter: tooltipFormatter},
      series: Data,
      plotOptions: {ohlc: {grouping: false, lineWidth: 2} }
    };

    chart = new Highcharts[backend](args);

    // Anything user wants to attach to graphic
    chart.custom_data = Options.custom_data;

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
        // To improve viewing speed we trigger redraw externally, not on every data packet
        startRedraw(graph, 250);
      } else if (data.set) {
        setGraphicData(ID, data);
      } else {
        updateGraphic(ID, data);
      };
    };

    s.onclose = function() {
      var chart = window.graphics[ID];
      if (chart) {
        chart.websocket && delete chart.websocket;
      } else {
        obj('#' + ID).innerText = "Error";
      }
    };
  };

  function updateGraphic(ID, Data) {
    var chart = window.graphics[ID];
    for (var s in Data) {
      var series = chart.get("series-" + s);
      if (s == "$marks") {
        updateMarks(series, Data[s])
      }
      else updateSeries(series, Data[s]);
    };
  };

  function updateSeries(series, points) {
    var count = points.length;

    for (var i = 0; i < count; i++) {
      series.addPoint(points[i], false);
    };
  };

  function setMarkLabels(marks) {
    return $.map(marks, function(m) {
      if (m.title == undefined) {}
      else if (m.title == null) m.dataLabels = {enabled:false};
      else m.dataLabels = {enabled:true};
      return m.mark_id;
    });
  };

  function updateMarks(series, marks) {
    var markIds = setMarkLabels(marks);
    var sData = series.data;
    var dataLen = sData.length;
    // scan old events, when ids compare equal, update
    for (var i = dataLen - 1; i >= 0 && markIds.length > 0; i --) {
      var cur = sData[i];
      var j = markIds.indexOf(cur.mark_id);
      // Positive j means sData[i] should be updated with marks[j]
      if (j >= 0) {
        markIds.splice(j, 1);
        var mark = marks.splice(j, 1)[0];
        cur.update(mark);
      };
    };
    // Now marks contain only new points
    $.map(marks, function(m) {series.addPoint(m)});
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
      if (graph.websocket) startRedraw(graph, interval);
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

  function genPlotLines(lines) {
    var plotLines = [];
    for (var name in lines) {
      var current = {color:"black", width:1, dashStyle:"dot"};
      var line = lines[name];

      current.label = {text:name, align:"center"};

      if (typeof(line) == "number") line = [line]
      current.value = line.shift();

      while (line.length > 0) {
        var option = line.shift();
        parsePlotLineOption(option, current);
      };
      plotLines.push(current);
    };

    return plotLines;
  };

  function parsePlotLineOption(option, plotLine) {
    if (!isNaN(option)) {
      // Numeric option -- assume it is line width
      plotLine.width = option;
      return plotLine;
    };
    var x = 0;
    switch (option) {
      // label position
      case "left":
        x = 60;
      case "right":
        x -= 30;
      case "center":
        plotLine.label.align = option;
        plotLine.label.x = x;
        break;

      // dash style
      case "solid":
        plotLine.dashStyle = "Solid"; break;
      case "short_dash":
        plotLine.dashStyle = "ShortDash"; break;
      case "short_dot":
        plotLine.dashStyle = "ShortDot"; break;
      case "short_dash_dot":
        plotLine.dashStyle = "ShortDashDot"; break;
      case "short_dash_dot_dot":
        plotLine.dashStyle = "ShortDashDotDot"; break;
      case "dot":
        plotLine.dashStyle = "Dot"; break;
      case "dash":
        plotLine.dashStyle = "Dash"; break;
      case "long_dash":
        plotLine.dashStyle = "LongDash"; break;
      case "dash_dot":
        plotLine.dashStyle = "DashDot"; break;
      case "long_dash_dot":
        plotLine.dashStyle = "LongDashDot"; break;
      case "long_dash_dot_dot":
        plotLine.dashStyle = "LongDashDotDot"; break;
        
      default:
        // Not width, not align, not style. Assume it is color.
        plotLine.color = option;
    };
    return plotLine;
  };

  function setVisibility(Field, Value, Visible) {
    var hidden;
    var allgraphics = window.graphics;

    if (typeof(Visible) == "boolean") hidden = !Visible
    else hidden = !Visible.checked;

    for (var id in allgraphics) {
      if (allgraphics[id].custom_data && allgraphics[id].custom_data[Field] == Value)
        obj("#" + id).hidden = hidden;
    };
  };


  window.Graphic = {
    autoHeight: autoHeight,
    render: renderGraphic,
    ws_request: requestWsGraphic,
    setVisibility: setVisibility
  };
  window.graphics = {};
})();
