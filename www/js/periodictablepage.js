const APP = {};

window.onload = function()
//function initJS ()
{
    SetEventHandlers();

    APP.periodictable = new PeriodicTable();

    APP.display = new PeriodicTableDisplay(APP.periodictable, "periodictable", "infoboxbackground", "infobox");

    APP.filterinputs =
	{
		name: document.getElementById("namefilter"),
		atomicnumber: document.getElementById("atomicnumberfilter"),
		symbol: document.getElementById("symbolfilter"),
		category: document.getElementById("categoryfilter"),
		group: document.getElementById("groupfilter"),
		period: document.getElementById("periodfilter"),
		block: document.getElementById("blockfilter")
	};
	document.getElementById("myelmBox").style.display  = "none";

	
}


function SetEventHandlers()
{
    document.getElementById("btnApplyFilter").onclick = ApplyFilter;
    document.getElementById("btnClearFilter").onclick = ClearFilter;
    document.getElementById("colorblock").onclick = ColorByBlock;
    document.getElementById("colorcategory").onclick = ColorByCategory;
}


function ColorByBlock()
{
    this.blur();

    document.getElementById("categorykey").style.display = "none";
    document.getElementById("blockkey").style.display = "inline";

    APP.display.ColorByBlock();
}


function ColorByCategory()
{
    this.blur();

    document.getElementById("blockkey").style.display = "none";
    document.getElementById("categorykey").style.display = "inline";

    APP.display.ColorByCategory();
}


function ApplyFilter()
{
    filtercriteria =
    {
        name: APP.filterinputs.name.value,
        atomicnumber: APP.filterinputs.atomicnumber.value,
        symbol: APP.filterinputs.symbol.value,
        category: APP.filterinputs.category.value,
        group: APP.filterinputs.group.value,
        period: APP.filterinputs.period.value,
        block: APP.filterinputs.block.value
    };

    APP.periodictable.ApplyFilter(filtercriteria)
}


function ClearFilter()
{
    APP.filterinputs.name.value = "";
    APP.filterinputs.atomicnumber.value = "";
    APP.filterinputs.symbol.value = "";
    APP.filterinputs.category.value = "";
    APP.filterinputs.group.value = "";
    APP.filterinputs.period.value = "";
    APP.filterinputs.block.value = "";

    APP.periodictable.ClearFilter();
}

