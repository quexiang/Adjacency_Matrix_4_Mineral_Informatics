class PeriodicTable
{
	constructor()
	{
		this._rowcount = 10;
		this._columncount = 19;
        this._data = periodictabledata;
        this._FilterChangedEventHandlers = [];
    }

    //---------------------------------------------------
    // PROPERTIES
    //---------------------------------------------------

	get columncount() { return this._columncount };
	get rowcount() { return this._rowcount; };
	get data() { return this._data; };

	//-----------------------------------------------------------------
    // METHODS
    //-------------------------------------------------------------------

	AddFilterChangedEventHandler(handler)
	{
		this._FilterChangedEventHandlers.push(handler);
	}


	ApplyFilter(filtercriteria)
	{
		const changed = [];

		for(let element of this._data)
		{
			const filterresults = [];

			filterresults.push(this._Match(element.name, filtercriteria.name, true));
			filterresults.push(this._Match(element.atomicnumber, filtercriteria.atomicnumber, false));
			filterresults.push(this._Match(element.symbol, filtercriteria.symbol, true));
			filterresults.push(this._Match(element.category, filtercriteria.category, false));
			filterresults.push(this._Match(element.group, filtercriteria.group, false));
			filterresults.push(this._Match(element.period, filtercriteria.period, false));
			filterresults.push(this._Match(element.block, filtercriteria.block, false));

			const show = filterresults.every( b => b === true );

			if(show === true)
			{
				if(element.visible === false)
				{
					element.visible = true;
					changed.push(element);
				}
			}
			else
			{
				if(element.visible === true)
				{
					element.visible = false;
					changed.push(element);
				}
			}
		}

		this._FireFilterChangedEvent(changed);
	}


	ClearFilter()
	{
		const changed = [];

		for(let element of this._data)
		{
			if(element.visible === false)
			{
				element.visible = true;
				changed.push(element);
			}
		}

		this._FireFilterChangedEvent(changed);
	}


	GetElement(atomicnumber)
	{
		return this._data[atomicnumber - 1];
	}

	//-----------------------------------------------------------------
    // FUNCTIONS
    //-------------------------------------------------------------------

    _FireFilterChangedEvent(changed)
    {
        this._FilterChangedEventHandlers.forEach(function (Handler) { Handler(changed); });
    }


	_Match(value, criteria, wildcard)
	{
		let match = true;

		if(criteria.length > 0)
		{
			if(wildcard)
			{
				match = value.toString().toLowerCase().includes(criteria.toString().toLowerCase());
			}
			else
			{
				match = (value.toString().toLowerCase() === criteria.toString().toLowerCase());
			}
		}

		return match;
	}
}
