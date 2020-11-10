namespace Date {
    pub type alias Date = { year: Int, month: Month, day: Int }

    pub enum Month {
        case January
        case February
        case March
        case April
        case May
        case June
        case July
        case August
        case September
        case October
        case November
        case December
    }

    pub enum DayOfWeek {
        case Monday
        case Tuesday
        case Wednesday
        case Thursday
        case Friday
        case Saturday
        case Sunday
    }

    // TODO make pure
    def jmonthToMonth(jmonth: Int): Option[Month] & Impure = {
        import get java.util.Calendar:JANUARY as getJanuary;
        import get java.util.Calendar:FEBRUARY as getFebruary;
        import get java.util.Calendar:MARCH as getMarch;
        import get java.util.Calendar:APRIL as getApril;
        import get java.util.Calendar:MAY as getMay;
        import get java.util.Calendar:JUNE as getJune;
        import get java.util.Calendar:JULY as getJuly;
        import get java.util.Calendar:AUGUST as getAugust;
        import get java.util.Calendar:SEPTEMBER as getSeptember;
        import get java.util.Calendar:OCTOBER as getOctober;
        import get java.util.Calendar:NOVEMBER as getNovember;
        import get java.util.Calendar:DECEMBER as getDecember;
        let map = Map#{
            getJanuary()-> January,
            getFebruary() -> February,
            getMarch() -> March,
            getApril() -> April,
            getMay() -> May,
            getJune() -> June,
            getJuly() -> July,
            getAugust() -> August,
            getSeptember() -> September,
            getOctober() -> October,
            getNovember() -> November,
            getDecember() -> December
        };
        Map.get(jmonth, map)
    }

    def ignore(_x: a): Unit = ()

    pub def ignoreAll(): Unit = {
        ignore(jmonthToMonth)
    }
}