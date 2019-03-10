ServerView : Singleton {
	classvar panelTypes, <>widgets, <actions, <>border=true,
	showOnBoot=true, serverViewShown=false;
	var <>view, <window, widgetLayout, server, <>widgets, containers;

	*initClass {
		panelTypes = IdentityDictionary();
		widgets = List[ServerStatusWidget, ScopeWidget, HistoryWidget, VolumeWidget, RecordWidget];
		actions = IdentityDictionary();

		ServerBoot.add(this);
		Platform.makeServerWindowAction = {
			this.doOnServerBoot();
		}
	}

	*doOnServerBoot {
		|server|
		if (showOnBoot && serverViewShown.not) {
			serverViewShown = true;
			ServerView().front;
		};

		if (Server.all.select(_.serverRunning).size == 0) {
			// If no other servers running, set this one to display
			ServerView().server = server;
		}
	}

	*default {
		^Server.default.name;
	}

	server {
		var runningServers = Server.all.select(_.serverRunning).asArray;
		^server
			?? { if (runningServers.size == 1) { runningServers[0] } }
			?? { Server.default }
			?? { Server.local }
	}

	set {}

	close {
		{ window.close() }.defer;
	}

	front {
		if (server.isNil) {
			//server = Server.default;
		};
		if (view.isNil) {
			this.createView();
		};
		view.front;
	}

	createView {
		window = Window(bounds:Rect(0, 0, 265, 200), resizable:true, border:border);
		window.autoRememberPosition(\ServerView);
		view = window.view.minWidth_(265).minHeight_(200);
		view.keyDownAction_({
			|v, char|
			actions[char.asSymbol].value(this);
		});
		if (border.not) {
			view.layout_(HLayout(
				WindowHandleView().maxWidth_(10).minWidth_(10),
				widgetLayout = VLayout()
			).spacing_(0).margins_(2));
		}{
			view.layout_(HLayout(
				widgetLayout = VLayout()
			).spacing_(0).margins_(2));
		};

		widgetLayout.margins = 3;
		widgetLayout.spacing = 1;
		view.onClose_(this.onClose(_));
		this.populateView();
	}

	populateView {
		containers = List();
		widgets = List();

		([ServerSelectorWidget] ++ this.class.widgets).do {
			|wClass, i|
			var container, widget;
			widget = wClass.new(this.server, this).view();
			widgets.add(widget);
			container = View().layout_(
				VLayout(
					widget,
					if (i != this.class.widgets.size()) {
						UserView().fixedHeight_(10).drawFunc_({
							|v|
							var y = (v.bounds.height / 2).round + 0.5;
							Pen.strokeColor = Color.grey(0.5, 0.2);
							Pen.width = 1;
							Pen.line(2@y, (v.bounds.width - 2)@y);
							Pen.stroke();
						})
					}
				).spacing_(1).margins_(0)
			).canFocus_(true);
			containers.add(container);
		};

		containers.do({
			|w|
			widgetLayout.add(w)
		});

		widgetLayout.add(nil);
	}

	onClose {
		|v|
		window = widgetLayout = view = nil;
		actions.clear;
	}

	clearView {
		if (view.notNil) {
			containers.do(_.remove);
		}
	}

	registerKeyAction {
		| key, action |
		// if (actions[key].notNil) {
		// 	"ServerView - Overriding an existing action for key %".format(key).warn;
		// };
		actions[key] = action;
	}

	server_{
		|newServer|
		if (server != newServer) {
			{
				server = newServer;
				if (view.notNil) {
					this.clearView();
					this.populateView();
				}
			}.defer();
		}
	}
}

ServerWidgetBase {
	var server, parent, buttonColor, faintGreen, faintRed, faintYellow, faintBlue,
	brightBlue, brightGreen, brightRed, highlightColor, fontface;

	*new {
		|server, parent|
		^super.newCopyArgs(server, parent).init();
	}

	*brightenColor {
		|inColor, amt|
		var color = Color.fromArray(inColor.asArray);
		color.red = (color.red + amt).linlin(0,1,0,1);
		color.green = (color.green + amt).linlin(0,1,0,1);
		color.blue = (color.blue + amt).linlin(0,1,0,1);
		^color
	}

	init {
		var mod = if (QtGUI.palette.window.asHSV[2] > 0.5) {-0.2} {0.2};
		buttonColor = QtGUI.palette.button;
		faintGreen = ServerWidgetBase.brightenColor(buttonColor.blend(Color.green, 0.2), mod);
		faintRed = ServerWidgetBase.brightenColor(buttonColor.blend(Color.red, 0.25), mod);
		faintYellow = ServerWidgetBase.brightenColor(buttonColor.blend(Color.yellow, 0.25), mod);
		faintBlue = ServerWidgetBase.brightenColor(buttonColor.blend(Color.blue, 0.25), mod);

		brightBlue = Color.hsv(0.555, 1, 0.6 + mod);
		brightGreen = Color.hsv(0.277, 1, 0.6 + mod);
		brightRed = Color.hsv(0.01, 1, 0.6 + mod);

		highlightColor = Color.grey(0.7 + mod);

		this.actions.do({
			|action|
			parent.registerKeyAction(action.key, action.action);
		});
	}

	font {
		| size=12, bold=false |
		if (fontface.isNil) {
			var fonts = Font.availableFonts;
			if (fonts.includesEqual("M+ 2c")) {
				fontface = "M+ 2c";
			} {
				if (fonts.includesEqual("Helvetica Neue")) {
					fontface = "Helvetica Neue"
				} {
					fontface = Font.defaultSansFace
				}
			};
		}
		^Font(fontface, size, bold);
	}

	actions { ^[] }

	view { this.subclassResponsibility('view') }
}

ServerViewAction {
	var <name, <key, <action;
	*new { |name, key, action| ^super.newCopyArgs(name, key, action) }
	value { |...args| action.value(*args) }
	asString { ^name }
}

ServerSelectorWidget : ServerWidgetBase {
	var serverList, view, runningText, bootButton, defaultButton, optionsMenu, optionsView, controller, serverQuitView;
	var connections;

	actions {
		var actions = [
			ServerViewAction("Options", 'o', {
				if (optionsView.isNil) {
					optionsView = ServerOptionsGui(server);
					optionsView.parent.autoRememberPosition(\ServerOptionsGui);
					optionsView.parent.onClose_({ optionsView = nil })
				};
			}),

			ServerViewAction("Query nodes", 'n',
				{ server.queryAllNodes(true) }),

			ServerViewAction("Level meters", 'l',
				{ server.tryPerform(\meter) }),

			ServerViewAction("Tree view", 'p',
				{ if(server.serverRunning) { TreeSnapshotView(server).autoUpdate().front() } }),

			ServerViewAction("Scope", 's',
				{
					var scopeWin = server.scope(server.options.numOutputBusChannels);
					scopeWin.scopeView.waveColors = 10.collect {
						| i |
						var h, s, v, a;
						#h, s, v, a = brightBlue.asHSV();
						h = (h + (i * 0.68)).mod(1).min(1).max(0);
						Color.hsv(h, s, v, a);
					};
				}
			),

			ServerViewAction("Frequency scope", 'f',
				{ server.freqscope }),

			ServerViewAction("Dump OSC", 'd',
				{ if (server.isLocal or: { server.inProcess }) { server.dumpOSC((server.dumpMode + 1) % 2) } }),

			ServerViewAction("Mute", 'm',
				{ if (server.volume.isMuted) { server.unmute } { server.mute } }),

			ServerViewAction("Reset volume", '0',
				{ server.volume = 0 }),
		];

		actions = [

			ServerViewAction("Boot", 'b',
				{ if(server.serverRunning.not) { server.boot } }),

			ServerViewAction("Reboot", 'none',
				{ if(server.serverRunning.not) { server.reboot } }),

			ServerViewAction("Quit", 'none',
				{ if(server.serverRunning) { server.quit } }),
		] ++ actions;

		actions = actions ++ [
			ServerViewAction("Kill all servers", 'none',
				{ Server.killAll }),
			ServerViewAction("Close Server View", 27.asAscii.asSymbol,
				{ parent.window.rememberPosition(\ServerView); parent.close() }),

		];

		^actions
	}

	menuActions {
		^[
			MenuAction("Query nodes", {
				server.queryAllNodes(true)
			}).shortcut_("n"),

			MenuAction("Level meters", {
				server.tryPerform(\meter)
			}).shortcut_("l"),

			MenuAction("Tree view", {
				if(server.serverRunning) { TreeSnapshotView(server).autoUpdate().front() }
			}).shortcut_("p"),

			MenuAction("Scope", {
				var scopeWin = server.scope(server.options.numOutputBusChannels);
				scopeWin.scopeView.waveColors = 10.collect {
					| i |
					var h, s, v, a;
					#h, s, v, a = brightBlue.asHSV();
					h = (h + (i * 0.68)).mod(1).min(1).max(0);
					Color.hsv(h, s, v, a);
				};
			}).shortcut_("s"),

			MenuAction("Frequency scope", {
				server.freqscope
			}).shortcut_("f"),

			MenuAction("Dump OSC", {
				|m|
				if (server.isLocal or: { server.inProcess }) {
					server.dumpOSC((server.dumpMode + 1) % 2);
					m.checked = (server.dumpMode > 0);
				}
			}).shortcut_("d"),

			MenuAction("Mute", {
				|m|
				if (server.volume.isMuted) { server.unmute } { server.mute };
				m.checked = server.volume.isMuted;
			}).checked_(server.volume.isMuted),

			MenuAction("Reset volume", {
				server.volume = 0
			}).shortcut_("0"),

			MenuAction("Options...", {
				if (optionsView.isNil) {
					optionsView = ServerOptionsGui(server);
					optionsView.parent.autoRememberPosition(\ServerOptionsGui);
					optionsView.parent.onClose_({ optionsView = nil })
				};
			}).shortcut_("o"),

			MenuAction.separator,

			MenuAction("Kill all servers", { Server.killAll }),
			MenuAction("Close Window", {
				parent.window.rememberPosition(\ServerView); parent.close()
			})
		]
	}

	makeServerMenu {
		var menu, button, onServersChanged;
		var serverRunningConnection;

		button = Button()
					.canFocus_(false)
					.fixedHeight_(26)
					.fixedWidth_(240);

		onServersChanged = {
			|what, obj, server|
			var string, serverName, color, displayedServer;

			displayedServer = parent.server;

			serverRunningConnection.free;
			serverRunningConnection = displayedServer.signal(\serverRunning).connectTo(onServersChanged).defer;

			serverName = "% %".format(
				displayedServer.serverRunning.if("◎", "◉");
				displayedServer.name.asString.toUpper
			);

			string = "% (%)".format(
				serverName,
				displayedServer.serverRunning.if("running", "stopped")
			);
			color = displayedServer.serverRunning.if(faintGreen, QtGUI.palette.buttonText.alpha_(0.6));

			button.states = [[
				string,
				color,
				Color.clear
			]];

			button.font = this.font(18, bold: displayedServer.serverRunning);
		};

		connections = connections.addAll([
			Server.signal(\serverAdded).connectTo(onServersChanged).defer,
			parent.server.signal(\default).connectTo(onServersChanged).defer // all servers get notified, so no need to register everywhere
		]);

		onServersChanged.();
		MainMenu.tryPerform(\initBuiltInMenus);
		button.menu = MainMenu.serversMenu;

		{
			connections.add(
				MainMenu.serversMenu.signal(\triggered)
				.connectTo({
					|obj, what, action|
					Server.all.detect({ |s| action.string.contains(s.name.asString) }) !? {
						|s|
						parent.server = s;
						onServersChanged.();
					}
				})
			);
		}.defer;

		^button;
	}

	makeOptionsMenu {
		var button;
		button = Button()
					.fixedSize_(32@20)
					.states_([[nil, Color.clear, Color.grey.alpha_(0.2)]])
					.canFocus_(false)
					.iconSize_(16);

		if (\Material.asClass.notNil) {
			button.icon_(Material("settings", 18, color:QtGUI.palette.buttonText.alpha_(0.5)))
		} {
			button.fixedSize_(40@20);
			button.states_([["☰", QtGUI.palette.buttonText.alpha_(0.5), QtGUI.palette.button.alpha_(0.2)]])
		};

		button.menu = Menu(*this.menuActions);
		^button;
	}

	makeServerMenuOld {
		var serverList, servers;

		servers = Server.all.asArray;
		serverList = PopUpMenu();
		serverList.items = Server.all.collect({
			| s |
			s.name;
		});
		serverList.value = servers.indexOf(server);
		serverList.action = {
			|v|
			parent.server = servers[v.value];
		};
		serverList.maxHeight_(18);
		serverList.font = this.font(12, false);

		^serverList
	}

	makeOptionsMenuOld {
		var optionsMenu = PopUpMenu().maxHeight_(18).maxWidth_(22);

		optionsMenu.allowsReselection = true;
		optionsMenu.items = [""] ++ this.actions.collectAs(_.name, Array);
		optionsMenu.action_({
			|v|
			var name = v.items[v.value];
			this.actions.detect({ |action| action.name == name }).value(server)
		});

		^optionsMenu
	}

	view {
		var menus, menuSupport = \Menu.asClass.notNil;

		controller = (SimpleController(server)
			// .put(\serverRunning, this.onRunning(_,_))
			.put(\default, this.onDefault(_,_))
		);

		serverList 		= menuSupport.if({ this.makeServerMenu }, { this.makeServerMenuOld });
		optionsMenu		= menuSupport.if({ this.makeOptionsMenu }, { this.makeOptionsMenuOld });

		view = View();
		view.layout_(
			HLayout(
				[serverList, \align: \left],
				defaultButton = (Button()
					.action_(this.defaultAction(_))
					.maxHeight_(18).maxWidth_(22)
					.canFocus_(false)
					.font_(this.font(12, true))
				),
				nil,
				optionsMenu
			).margins_(0).spacing_(0)
		);

		this.onDefault();

		view.onClose_({ controller.remove() });

		^view;
	}

	bootAction {
		if (server.serverRunning.not) {
			server.boot;
		}
	}

	defaultAction {
		Server.default = server;
	}

	killAction {
		Server.killAll;
	}

	onDefault {
		if (Server.default == server) {
			defaultButton
				.states_([["D", faintGreen, Color.grey.alpha_(0.2)]])
				.font_(this.font(12, true))
		} {
			defaultButton
				.states_([["D", nil, Color.grey.alpha_(0.2)]])
				.font_(this.font(11, false, Color.grey.alpha_(0.2)))
		}
	}

}

ServerStatusWidget : ServerWidgetBase {
	classvar <allCounters;
	var view, controller, columns=3, trackedCounts, counters;

	*initClass {
		allCounters = List[\numSynths, \numGroups, \numUGens, \avgCPU, \numSynthDefs, \peakCPU, \sampleRate, \actualSampleRate];
	}

	view {
		var counterViews;
		controller = (SimpleController(server)
			.put(\counts, this.onCounts(_,_))
		);

		counters = IdentityDictionary();
		trackedCounts = [\numSynths, \numGroups, \avgCPU, \numUGens, \numSynthDefs, \peakCPU];
		trackedCounts.do {
			|countName|
			counters[countName] = this.perform( ("create_" ++ countName).asSymbol);
		};
		counterViews = List();
		trackedCounts.do({|c|
			counters[c].view.background_(ServerWidgetBase.brightenColor(QtGUI.palette.window, -0.02));
			counterViews.add([ counters[c].view, columns:counters[c].span ]);
			(counters[c].span - 1).do { counterViews.add(nil) }
		});
		counterViews = counterViews.clump(columns);

		view = View().layout_(
			GridLayout.rows(*counterViews).spacing_(1).margins_(0)
		);

		view.onClose_({ controller.remove() });

		^view;
	}

	onCounts {
		allCounters.do {
			|counterName|
			counters[counterName] !? { counters[counterName].value = server.perform(counterName) };
		}
	}

	create_numUGens {
		^GraphCounter("UGENS", "", this.font(8), brightBlue.alpha_(0.6), 0);
	}

	create_numSynths {
		^GraphCounter("SYNTHS", "", this.font(8), brightBlue.alpha_(0.6), 0);
	}

	create_numGroups {
		^NumberCounter("GROUPS", "", this.font(8));
	}

	create_numSynthDefs {
		^NumberCounter("SYNTHDEFS", "", this.font(8));
	}

	create_avgCPU {
		^GraphCounter("AVG CPU", "%", this.font(8), brightGreen, 0, 100, brightRed.alpha_(0.6));
	}

	create_peakCPU {
		^GraphCounter("PEAK CPU", "%", this.font(8), brightGreen, 0, 100, brightRed.alpha_(0.6));
	}
}

NumberCounter {
	var name, units, font, <view, heading, number, <>span=1;

	*new {
		|name, units, font|
		^super.newCopyArgs(name, units, font).init;
	}

	init {
		var mod = if (QtGUI.palette.window.asHSV[2] > 0.5) {-0.5} {0.3};
		view = View().layout_(VLayout(
			heading = (StaticText()
				.string_(name)
				.font_(font)
				.stringColor_(Color.grey)
				.align_(\center)
			),
			number = (StaticText()
				.font_(font.boldVariant.size_(font.size + 2))
				.stringColor_(Color.grey(0.7 + mod))
				.align_(\center)
			)
		).margins_(0).spacing_(0));
	}

	value_{
		|val|
		number.string = (val.round(0.1).asString + units);
	}
}

GraphCounter {
	var name, units, font, color, <>min, <>max, maxColor, <>minFixed=false, <>maxFixed=false, historySize, reverse,
	<view, heading, number, history, <>span=1, <>round=0.1,
	<>xWarp
	;

	*new {
		|name, units, font, color, min, max, maxColor, historySize=20, reverse=false|
		^super.newCopyArgs(name, units, font, color, min, max, maxColor, min.notNil, max.notNil, historySize, reverse)
		.init;
	}

	init {
		var mod = if (QtGUI.palette.window.asHSV[2] > 0.5) {-0.5} {0.3};
		xWarp = ControlSpec(0, 1);

		history = LinkedList.newFrom(0 ! historySize);
		view = UserView().layout_(VLayout(
			heading = (StaticText()
				.string_(name)
				.font_(font)
				.stringColor_(Color.grey)
				.align_(\center)
			),
			number = (StaticText()
				.font_(font.boldVariant.size_(font.size + 2))
				.stringColor_(Color.grey(0.7 + mod))
				.align_(\center)
				.visible_(units.notNil)
			)
		).margins_(0).spacing_(0))
		.drawFunc_({
			|v|
			var b = v.bounds, size;
			size = history.size()- 1;
			Pen.push();

			Pen.width = 0;
			if (reverse) {
				Pen.scale(b.width, b.height.neg);
				Pen.translate(0, -1);
				Pen.moveTo(0@0);
			} {
				Pen.scale(b.width.neg, b.height.neg);
				Pen.translate(-1, -1);
				Pen.moveTo(0@0);
			};

			history.do {
				|val, i|
				var x, y;
				x = xWarp.map(i / size.asFloat);
				y = (val.linlin(min, max, 0, 1)).min(1).max(0);
				Pen.lineTo(x@y);
			};
			Pen.lineTo(1@0);
			Pen.lineTo(0@0);
			if (maxColor.notNil && (history.first.linlin(min, max, 0, 1) > 0.9)) {
				Pen.fillColor = maxColor.alpha_(0.3);
				Pen.strokeColor = maxColor.blend(Color.white, 0.1).alpha_(0.8);
			} {
				Pen.fillColor = color.alpha_(0.3);
				Pen.strokeColor = color.blend(Color.white, 0.1).alpha_(0.8);
			};
			Pen.draw(3);

			Pen.pop();
		})
		.mouseDownAction_(this.resetMinMax(_));
		this.resetMinMax();
	}

	resetMinMax {
		if (maxFixed.not) { max = -9999999 };
		if (minFixed.not) { min = 9999999 };
		view.refresh();
	}

	clear {
		history = history.collect(0);
		this.value = 0;
	}

	value_{
		|val|
		history.pop();
		history.addFirst(val);

		if ((val < min) && minFixed.not) {
			min = val;
		};
		if ((val > max) && maxFixed.not) {
			max = val;
		};

		number.string = (val.round(round).asString + units);
		view.refresh();
	}
}

VolumeWidget : ServerWidgetBase {
	var view, controller, volumeNum, volumeSlider, muteButton, volSpec, <>height=18;

	view {
		controller = SimpleController(server.volume)
		.put(\amp, {|changer, what, vol|
			{
				volumeNum.value_(vol.round(0.01));
				volumeSlider.value_(volSpec.unmap(vol));
			}.defer
		})
		.put(\mute, {|changer, what, flag|
			{
				muteButton.value_(flag.binaryValue);
			}.defer
		})
		.put(\ampRange, {|changer, what, min, max|
			volSpec = [min, max, \db].asSpec;
			volumeSlider.value_(volSpec.unmap(server.volume.volume));
		});
		volSpec = [server.volume.min, server.volume.max, \db].asSpec;

		view = View().layout_(
			VLayout(
				HLayout(
					StaticText().font_(this.font(9)).string_("VOL:  "),
					volumeSlider = (Slider()
						.action_({ |v| server.volume_(volSpec.map(v.value).round(0.1)) })
						.orientation_(\horizontal)
						.maxHeight_(height)
						.thumbSize_(height - 5)
						.value_(volSpec.unmap(server.volume.volume))
					),
					volumeNum = (NumberBox()
						.action_({ |v| server.volume_(v.value.round(0.1)) })
						.font_(this.font().boldVariant)
						.stringColor_(QtGUI.palette.windowText).normalColor_(QtGUI.palette.windowText)
						.maxWidth_(48)
						.maxHeight_(height - 2)
						.value_(server.volume.volume)
					),
					muteButton = (Button()
						.action_({ |v| (v.value == 1).if({ server.mute }, {server.unmute}) })
						.font_(this.font())
						.maxHeight_(height - 2).maxWidth_(22)
						.canFocus_(false)
						.states_([
							["M"],
							["M", nil, faintRed]
						])
					)
				).margins_(0).spacing_(0),

			).margins_(0).spacing_(0)
		);
		view.onClose_({ controller.remove() });

		^view;
	}
}

RecordWidget : ServerWidgetBase {
	classvar <recIcon, <stopIcon;
	var view, pathString, <recPath, timeString, timeRoutine, busInput, channelsInput, channelsInputText;

	*initClass {
		recIcon = Image(16).draw({ this.drawRec(14) });
		stopIcon = Image(16).draw({ this.drawStop(16) });
	}

	isRecording {
		^server.recorder.isRecording
	}

	*drawStop {
		| size |
		Pen.stringCenteredIn("▨", Rect(0,1,size,size),
			font: Font(size:14),
			color: QtGUI.palette.buttonText
		);
	}

	*drawRec {
		| size |
		Pen.stringCenteredIn("●", Rect(0,1,size,size),
			font: Font(size:14),
			color: Color.hsv(0, 0.8, 0.9)
		);
	}

	buttonClicked {
		|button|
		var bus, channels;

		bus = busInput.value.asInteger;
		channels = channelsInput.value.asInteger;

		if (this.isRecording) {
			server.stopRecording();
		} {
			server.record(path: this.resolvedRecPath(), bus:bus, numChannels:channels);
		};

		view.refresh;
	}

	resolvedRecFolder {
		var actualPath = recPath ?? { thisProcess.platform.recordingsDir };
		^actualPath;
	}

	resolvedRecPath {
		var timestamp;
		var dir = this.resolvedRecFolder();
		timestamp = Date.localtime.stamp;
		^dir +/+ server.recorder.filePrefix ++ timestamp ++ "." ++ server.recHeaderFormat;
	}

	recPath_{
		|path|
		recPath = path;
		this.updateRecPathString();
	}

	updateRecPathString {
		var path = PathName(this.resolvedRecFolder());
		pathString.string = " ..." +/+ path.folderName +/+ path.fileName;
	}

	view {
		var recButton, label, path, openButton, showButton;

		label = StaticText().string_("REC:").font_(this.font(9));

		pathString = (DragSink()
			.font_(this.font(8))
			.stringColor_(QtGUI.palette.windowText.alpha_(0.7))
			.background_(QtGUI.palette.button.alpha_(0.8))
			.fixedHeight_(20)
		);
		pathString.mouseUpAction = {
			if (thisProcess.platformClass == OSXPlatform) {
				"open '%'".format(this.resolvedRecFolder()).unixCmdGetStdOut();
			}
		};
		pathString.canReceiveDragHandler = { View.currentDrag.asString.pathExists == \folder };
		pathString.receiveDragHandler = {
			this.recPath = View.currentDrag.asString;
		};
		this.updateRecPathString();

		busInput = (NumberBox()
			.font_(this.font(8))
			.stringColor_(QtGUI.palette.windowText.alpha_(0.7))
			.background_(QtGUI.palette.button.alpha_(0.8))
			.align_(\right)
			.fixedWidth_(20)
			.fixedHeight_(20)
		);
		busInput.value = 0;

		channelsInputText = StaticText()
			.string_("⁝")
			.font_(this.font(12))
			.stringColor_(QtGUI.palette.windowText.alpha_(0.7))
			.align_(\center)
			.fixedWidth_(6)
			.fixedHeight_(20);
		channelsInput = (NumberBox()
			.font_(this.font(8))
			.stringColor_(QtGUI.palette.windowText.alpha_(0.7))
			.background_(QtGUI.palette.button.alpha_(0.8))
			.align_(\right)
			.fixedWidth_(20)
			.fixedHeight_(20)
		);
		channelsInput.value = server.options.numOutputBusChannels;

		timeString = (TextField()
			.font_(this.font(11, true))
			.align_(\right)
			.stringColor_(QtGUI.palette.windowText)
			.background_(Color.clear)
			.fixedWidth_(75)
			.fixedHeight_(20)
		);

		recButton = Button()
						.states_([[nil, Color.clear, Color.clear]])
						.fixedSize_(20@20)
						.canFocus_(false);
		recButton.icon = recIcon;

		recButton.mouseUpAction = this.buttonClicked(_);

		view = View().layout_(HLayout(
			label, pathString,
			5, timeString, HLayout(busInput, channelsInputText, channelsInput).spacing_(0).margins_(0), recButton
		).margins_(0).spacing_(9));

		server.signal(\recording).connectTo({
			|server, what, state|
			if (state) {
				recButton.icon = stopIcon;
			} {
				recButton.icon = recIcon;
			}
		}).defer.freeAfter(view);

		server.signal(\recordingDuration).connectTo({
			|server, what, dur|
			timeString.string = (dur).asTimeString(maxDays:0)[3..][0..4];
		}).defer.freeAfter(view);

		view.toFrontAction = view.toFrontAction.addFunc({
			this.updateRecPathString();
		});

		^view
	}
}

ScopeWidget : ServerWidgetBase {
	var view, <scopeView,
	inMetersView, <inMeters, inMetersMenu, inChannels, inChannelsActions,
	outMetersView, <outMeters, outMetersMenu, outChannels, outChannelsActions,
	scopeMenu, scopeChannels, scopeStyle, scopeMenu,
	meters, meterActions, prefConnections,
	<synth, levelSynth, levelsName, outresp, bus, rate=\audio, index=0,
	updateFreq=20, cycle=2048, dbList, dBLow, numRMSSamps, style=0, <outBus;

	*new {
		|...args|
		var me;
		me = super.new(*args);
		^me.init
	}

	init {
		super.init();
	}

	initPrefs {
		scopeChannels 	= Preference(\ScopeWidget, \scopeChannels, server.options.outDevice.asSymbol)
							.default_({ server.options.numOutputBusChannels })
							.spec_(ControlSpec(1, 32, step:1));
		scopeStyle	 	= Preference(\ScopeWidget, \scopeStyle)
							.default_({ 0 })
							.spec_(ControlSpec(0, 2, step:1));
		inChannels 		= Preference(\ScopeWidget, \inChannels, server.options.inDevice.asSymbol)
							.default_({ server.options.numInputBusChannels })
							.spec_(ControlSpec(1, 32, step:1));
		outChannels 	= Preference(\ScopeWidget, \outChannels, server.options.outDevice.asSymbol)
							.default_({ server.options.numOutputBusChannels })
							.spec_(ControlSpec(1, 32, step:1));
		dBLow 			= Preference(\ScopeWidget, \dBLow, server.options.outDevice.asSymbol)
							.default_(-60)
							.spec_(ControlSpec(-120, 120));

		prefConnections.free;

		prefConnections = ConnectionList [
			scopeChannels.signal(\value).connectTo(
				this.methodSlot("restartSynth()"),
				this.methodSlot("updateMenus()")
			),
			scopeStyle.signal(\value).connectTo(
				scopeView.methodSlot("style_(value)"),
				this.methodSlot("updateMenus()")
			),
			inChannels.signal(\value).connectTo(
				this.methodSlot("populateInMeters()"),
				this.methodSlot("restartSynth()"),
				this.methodSlot("updateMenus()")
			),
			outChannels.signal(\value).connectTo(
				this.methodSlot("populateOutMeters()"),
				this.methodSlot("restartSynth()"),
				this.methodSlot("updateMenus()")
			),
			dBLow.signal(\value).connectTo(
				{
					meters.do {
						|m|
						m.warning = -3.linlin(this.dBLow, 0, 0, 1);
						m.critical = -0.1.linlin(this.dBLow, 0, 0, 1);
					}
				},
				this.methodSlot("updateMenus()")
			)
		];
	}

	updateMenus {
		meterActions = [-40, -60, -80].collect {
			|db|
			MenuAction(db.asString, {
				|a|
				this.dBLow = db;
			}).checkable_(true).checked_(this.dBLow == db)
		};

		// input meters
		inMetersView.setContextMenuActions(*[
			meterActions,
			MenuAction.separator("Channels"),
			server.options.numInputBusChannels.collect({
				|i|
				i = i + 1;
				MenuAction(i.asString, {
					this.inChannels = i;
				}).checkable_(true).checked_(this.inChannels == i)
			})
		].flatten);

		// output meters
		outMetersView.setContextMenuActions(*[
			meterActions,
			MenuAction.separator("Channels"),
			server.options.numOutputBusChannels.collect({
				|i|
				i = i + 1;
				MenuAction(i.asString, {
					this.outChannels = i
				}).checkable_(true).checked_(this.outChannels == i)
			})
		].flatten);

		// scope
		scopeView.setContextMenuActions(*[
			MenuAction("Separate", 	{ this.scopeStyle = 0 }).checkable_(true).checked_(this.scopeStyle == 0),
			MenuAction("Layered", 	{ this.scopeStyle = 1 }).checkable_(true).checked_(this.scopeStyle == 1),
			MenuAction("XY", 		{ this.scopeStyle = 2 }).checkable_(true).checked_(this.scopeStyle == 2),

			MenuAction.separator("Channels"),

			server.options.numOutputBusChannels.collect({
				|i|
				i = i + 1;
				MenuAction(i.asString, {
					this.scopeChannels = i;
				}).checkable_(true).checked_(this.scopeChannels == i)
			})
		].flatten);
	}

	scopeChannels { ^min(server.options.numOutputBusChannels, scopeChannels.value.asInteger) }
	scopeChannels_{
		|v|
		scopeChannels.value = v;
	}

	scopeStyle { ^scopeStyle.value.asInteger }
	scopeStyle_{
		|v|
		scopeStyle.value = v;
	}

	inChannels { ^min(server.options.numInputBusChannels, inChannels.value.asInteger) }
	inChannels_{
		|v|
		inChannels.value = v;
	}

	outChannels { ^min(server.options.numOutputBusChannels, outChannels.value.asInteger) }
	outChannels_{
		|v|
		outChannels.value = v;
	}

	dBLow { ^dBLow.value.asInteger }
	dBLow_{
		|v|
		dBLow.value = v;
	}

	populateMeters {
		|parent, channels|
		var newMeters;

		parent.removeAll();
		newMeters = channels.collect {
			(LevelIndicator()
				.fixedWidth_(6 - (channels).linlin(2, 8, 0, 3).round(1))
				.minWidth_(2)
				.stepWidth_(1)
				.style_(1)
				.warning_(-6.dbamp)
				.critical_(-0.1.dbamp)
				.drawsPeak_(true)
			)
		};

		newMeters.do(parent.layout.add(_));
		^newMeters;
	}

	populateInMeters {
		inMeters = this.populateMeters(inMetersView, this.inChannels);
		meters = inMeters ++ outMeters;
	}

	populateOutMeters {
		outMeters = this.populateMeters(outMetersView, this.outChannels);
		meters = inMeters ++ outMeters;
	}

	view {
		var subviews,
			inMenuContext, outMenuContext, scopeContext;

		levelsName = (server.name ++ "OutputWidgetLevels");

		view = View().layout_(HLayout(
			inMetersView = View().layout_(HLayout().margins_(0).spacing_(0)),
			[scopeView = ScopeView(), stretch:2],
			outMetersView = View().layout_(HLayout().margins_(0).spacing_(0))
		).margins_(0).spacing_(1));

		this.initPrefs();

		scopeView.server = server;
		scopeView.canFocus = true;
		scopeView.background = QtGUI.palette.window;

		ServerTree.add(this, server);
		ServerQuit.add(this, server);

		if (server.serverRunning) {
			this.doOnServerTree();
		};

		scopeView.mouseWheelAction_({ |...args| this.mouseWheelAction(*args) });

		view.signal(\closed).connectTo({
			this.stopSynth();
			ServerTree.remove(this, server);
			ServerQuit.remove(this, server);
		}).oneShot;

		^view;
	}

	mouseWheelAction {
		| v, x, y, mods, xScroll, yScroll |
		if (scopeView.notNil) {
			if (xScroll != 0) {
				cycle = (cycle + (xScroll * 1)).linlin(100, 2048, 100, 2048);
				synth.setCycle(cycle);
			};
			if (yScroll != 0) {
				scopeView.yZoom = (scopeView.yZoom + (yScroll * 0.01)).linlin(1, 100, 1, 100);
			};
		}
	}

	playLevelSynth {
		outBus = Bus.control(server, (this.inChannels + this.outChannels) * 2);
		"Allocated levels outbus: %".format(outBus).postln;
		if (levelSynth.notNil) {
			levelSynth.free;
		};
		if (outresp.notNil) {
			outresp;
		};

		numRMSSamps = server.sampleRate / updateFreq;

		levelSynth = SynthDef(levelsName, {
			|out|
			var sig, imp, peak, max, output;
			sig = [
				SoundIn.ar((0..this.inChannels-1)),
				In.ar(0, this.outChannels)
			].flatten;

			imp = Impulse.ar(updateFreq);

			max = RunningMax.ar(sig.abs, Delay1.ar(imp));
			peak = Peak.ar(sig, Delay1.ar(imp)).lag(0, 3);

			output = [max, peak].flop.flat;

			Out.kr(out, output);
			SendReply.ar(imp, "/" ++ levelsName, output);
		}).play(RootNode(server), [\out, outBus], \addToTail);

		outresp = OSCdef(\widgetLevels, {
			|msg, t|
			{
				msg = msg[3..];
				msg.pairsDo({
					|val, peak, i|
					var meter;
					meter = meters[i * 0.5];

					if (meter.notNil) {
						meter.value = val.max(0.0).ampdb.linlin(this.dBLow, 0, 0, 1);
						meter.peakLevel = peak.ampdb.linlin(this.dBLow, 0, 0, 1);
					}
				})
			}.defer;
		}, ("/" ++ levelsName).asSymbol, server.addr).fix;
	}

	startSynth {
		if (synth.isNil) {
			synth = BusScopeSynth(server);
		};
		if (bus.isNil) {
			bus = Bus(rate, index, this.scopeChannels, server);
		};
		if (synth.isPlaying.not) {
			scopeView.stop();

			scopeView.server = server;
			scopeView.bufnum = synth.bufferIndex;
			scopeView.style = 1;
			scopeView.yZoom = 0.9;
			scopeView.waveColors = this.scopeChannels.collect {
				| i |
				var h, s, v, a;
				#h, s, v, a = brightBlue.asHSV();
				h = (h + (i * 0.68)).mod(1).min(1).max(0);
				Color.hsv(h, s, v, a);
			};

			scopeView.start();
			synth.play(2048, bus, cycle);
			this.playLevelSynth();
		}
	}

	stopSynth {
		scopeView.stop();
		synth.free; bus.free; outBus.free; levelSynth.free; outresp.free;
		levelSynth = synth = bus = outBus = nil;
	}

	restartSynth {
		if (server.serverRunning) {
			this.stopSynth();
			this.startSynth();
		}
	}

	doOnServerTree {
		this.initPrefs();
		this.updateMenus();
		this.populateInMeters();
		this.populateOutMeters();
		this.startSynth();
		this.updateMenus();
	}

	doOnServerQuit {
		this.stopSynth();
	}

	cmdPeriod {
		this.stopSynth();
		this.startSynth();
	}
}

HistoryWidget : ServerWidgetBase {
	var historyResponder, historySynth, counter;
	var cmdName = '/HistoryWidget';

	doOnServerTree {
		|bootingServer|
		if (bootingServer == server) {
			this.start();
		}
	}

	doOnServerQuit {
		|bootingServer|
		if (bootingServer == server) {
			this.stop;
		}
	}

	start {
		var channels = server.options.numOutputBusChannels ?? 2;

		historySynth = SynthDef("ampHistory_%".format(channels).asSymbol, {
			SendPeakRMS.kr(InFeedback.ar(0, channels), 6, 1, cmdName:cmdName);
		}).play(RootNode(server), addAction:\addToTail);

		historyResponder = OSCFunc({
			|msg|
			var peak, rms;
			msg = msg[3..].clump(2).flop;

			peak = msg[0];
			rms = msg[1];

			// { counter.value = peak.maxItem.ampdb.linlin(-60, 0, 0, 1) }.defer;
			{ counter.value = peak.maxItem }.defer;
		}, cmdName);

		historySynth.onFree({
			|freed|
			if (freed == historySynth) {
				historySynth = nil;
			}
		})
	}

	stop {
		historySynth !? { historySynth.free };
		historySynth = nil;

		historyResponder !? { historyResponder.free };
		historyResponder = nil;
	}

	view {
		var view;

		counter = GraphCounter("PEAK DB", nil, this.font(8), brightGreen, 0.0, 1.0, historySize:160);
		view = counter.view;
		view.minHeight_(40);

		if (server.serverBooting.not && server.serverRunning) {
			this.start();
		};

		ServerTree.add(this);
		ServerQuit.add(this);

		view.onClose = { this.stop() };

		^view
	}
}
