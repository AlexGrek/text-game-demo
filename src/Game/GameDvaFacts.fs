module GameDvaFacts

open Facts

type GameDvaFacts() =

    member val deadBodyFound =
        createFact "deadBodyFound" "Труп мужчины" """Труп мужчины на кухне. Убит огромным кухонным ножом. Удар в грудь, лужа крови...
        такое не забывается...
        """
    
    member val screamedInWindow =
        createFact "screamedInWindow" "Я кричала в окно" """Ты голая в панике орала в открытое окно. Логика где? Пиздец, это может плохо кончиться.
        """

    member val shkafSeen =
        createFact "shkafSeen" "В шкафу нет моей одежды" """Шкаф осмотрен. Ничего интересного, никаких моих вещей. 
        Единственное наблюдение - у владельца этого шкафа точно есть стиль. Или был..."""

    member val version =
        createFact "atLeastVersion" "Версия произошедшего - самозащита" """У меня есть хоть какая-то версия произошедшего. Что мне что-то подсыпали, я защищалась.
        Надеюсь, не только я в нее поверю.
        """

    member val phoneFprints =
        createFactDeniable "phoneFprints" "Отпечатки на телефоне" """Я оставила телефон со своими отпечатками в кармане у мертвеца. Может, стоит его забрать? 
        Даже не знаю. Все равно меня найдут. Лучше там, чем если его найдут у меня.
        """

    member val noWater =
        createFact "noWater" "Нет воды в водопроводе" """Горячей воды нет. и холодной тоже. Как тут люди живут вообще?
        """

    member val thrownIphone =
        createFact "thrownIphone" "Выброшенный в окно телефон" """Я выбросила телефон мертвеца в окно и разбила им чужую тачку. Все очень плохо.
        """
    
    member val afterlife =
        createFact "afterlife" "Жизнь после смерти" """Я отчетливо помню... удар... острая боль.. темнеет в глазах... 
        Я же умерла! Мне не показалось? Я уже была здесь, я видела это. Почему я... жива? Что со мной?
        """

    member val strangerKillsMe =
        createFact "strangerKillsMe" "Странный тип на верхнем этаже" "Я слышала выстрелы, и сама стала жертвой странного типа в капюшоне."