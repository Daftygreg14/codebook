using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab7
{
    public class Relacja
    {
        public enum EnumRodzajRelacji { Przyjaciel }
        
        private Dziecko _dziecko1, _dziecko2;
        private DateTime _dataRozpoczecia;
        private DateTime? _dataZakonczenia;
        private EnumRodzajRelacji _rodzajRelacji;

        public Relacja(Dziecko dziecko1, Dziecko dziecko2, EnumRodzajRelacji rodzajRelacji)
        {
            _dziecko1 = dziecko1;
            _dziecko2 = dziecko2;
            _dataRozpoczecia = DateTime.UtcNow;
            _rodzajRelacji = rodzajRelacji;
        }

        public bool CzyPrzyjazn()
        {
            return _rodzajRelacji == EnumRodzajRelacji.Przyjaciel;
        }
        
        public bool CzyTrwa()
        {
            return _dataZakonczenia == null || _dataZakonczenia > DateTime.UtcNow;
        }
        
        public Dziecko Dziecko1 { get { return _dziecko1; } }
        public Dziecko Dziecko2 { get { return _dziecko2; } }
        public DateTime DataRozpoczecia { get { return _dataRozpoczecia; } }
        public EnumRodzajRelacji RodzajRelacji { 
            get { return _rodzajRelacji; }
            set { _rodzajRelacji = value; }
        }
    }
}

