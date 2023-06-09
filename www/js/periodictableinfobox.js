class PeriodicTableInfoBox
{
	constructor(periodictable, infoboxbackgroundid, infoboxid)
	{
		this._periodictable = periodictable;
        this._infoboxid = infoboxid;
        this._infoboxbackgroundid = infoboxbackgroundid;

		document.onkeyup = (event) =>
		{
			if(event.key === "Escape" && document.getElementById(this._infoboxid).style.visibility === "visible")
			{
				this.Hide();
			}
		}

		document.getElementById("btnCloseInfoBox").onclick = () => this.Hide();
    }


	Hide()
	{
		document.getElementById(this._infoboxid).style.visibility = "hidden";
		document.getElementById(this._infoboxbackgroundid).style.visibility = "hidden";
	}


	Show(atomicnumber)
	{
		const element = this._periodictable.GetElement(atomicnumber);
		const ore_elements_73 = ['H', 'Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Na', 'Mg', 'Al', 'Si', 'P', 'S','Cl', 'K', 'Ca', 'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu',
	  'Zn', 'Ga', 'Ge','As', 'Se', 'Br', 'Rb', 'Sr', 'Y', 'Zr', 'Nb', 'Mo', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd','In', 'Sn', 'Sb', 'Te', 'I', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', 'Sm', 'Gd',
	  'Dy', 'Er','Yb', 'Hf', 'Ta', 'W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Th', 'U'];
		if (ore_elements_73.includes(element.symbol) == false){
  		document.getElementById("infoName").innerHTML = element.name + "_ is not one of the 73 mineral-forming elements.";
  		document.getElementById("infoAtomicNumber").innerHTML = element.atomicnumber;
  		document.getElementById("infoChemicalSymbol").innerHTML = element.symbol;
  		document.getElementById("infoCategory").innerHTML = element.category;
  		document.getElementById("infoAtomicWeightConventional").innerHTML = element.atomicweight;
  		document.getElementById("infoAtomicWeightStandard").innerHTML = element.atomicweightfull;
  		document.getElementById("infoOccurrence").innerHTML = element.occurrence;
  		document.getElementById("infoStateOfMatter").innerHTML = element.stateofmatter;
  		document.getElementById("infoGroup").innerHTML = element.group;
  		document.getElementById("infoPeriod").innerHTML = element.period;
  		document.getElementById("infoBlock").innerHTML = element.block;
  		document.getElementById(this._infoboxbackgroundid).style.visibility = "visible";
  		document.getElementById(this._infoboxid).style.visibility = "visible";
		}else{
  		//取出元素然后给  #node_elm_list<-  #edge_elm_list<-赋值，然后显示出邻接矩阵
  		var x = document.getElementById("myelmBox");
      if (x.style.display === "none") {
            x.style.display = "block";
            Shiny.setInputValue("elementNmae", element.symbol);
            document.getElementById("atomicnumberfilter").value = element.atomicnumber;
            document.getElementById("btnApplyFilter").click();
       } else {
          x.style.display = "none";
          document.getElementById("btnClearFilter").click();
      }
		}
	}
}
