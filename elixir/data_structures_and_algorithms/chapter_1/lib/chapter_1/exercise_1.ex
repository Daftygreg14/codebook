defmodule Chapter1.Exercise1 do
  defmodule Match do
    @moduledoc """
    League Graphs Edge
    """

    defstruct team1: nil, team2: nil, week: nil

    def build(teams, week) do
      [t1, t2] = Enum.sort(teams)

      %__MODULE__{
        team1: t1,
        team2: t2,
        week: week
      }
    end

    def already_played?(matches, match) do
      Enum.any?(matches, &equal?(&1, match))
    end

    defp equal?(%Match{team1: team1, team2: team2}, %Match{team1: team1, team2: team2}), do: true
    defp equal?(%Match{}, %Match{}), do: false
  end

  defmodule League do
    @moduledoc """
    League Graph
    """

    @previous_week -1
    @initial_week nil

    defstruct teams: [], matches: MapSet.new([])

    def build(teams, already_played) do
      teams
      |> initialize_league()
      |> build_already_played_matches(already_played)
      |> fulfill_missing_matches()
    end

    defp initialize_league(teams) do
      %__MODULE__{teams: teams}
    end

    defp build_already_played_matches(league, []), do: league
    defp build_already_played_matches(league, already_played) do
      matches =
        Enum.reduce(already_played, MapSet.new([]), fn {team1, teams}, acc ->
          Enum.reduce(teams, acc, fn team2, acc ->
            match = Match.build([team1, team2], @previous_week)
            MapSet.put(acc, match)
          end)
        end)

      %__MODULE__{league | matches: matches}
    end

    defp fulfill_missing_matches(league = %League{matches: already_played, teams: teams}) do
      matches =
        Enum.reduce(teams, already_played, fn team1, matches ->
          Enum.reduce(teams, matches, fn team2, acc ->
            add_match_to_league(team1, team2, acc)
          end)
        end)

      %League{league | matches: matches}
    end

    defp add_match_to_league(team, team, matches), do: matches
    defp add_match_to_league(team1, team2, matches) do
      match = Match.build([team1, team2], @initial_week)

      if Match.already_played?(matches, match) do
        matches
      else
        MapSet.put(matches, match)
      end
    end
  end

  @start_week 1
  @teams [:szakale, :lwy, :orly, :bobry, :tyrgysy, :skunksy]
  @played_matches %{
    szakale: [:lwy, :orly],
    lwy: [:bobry, :skunksy],
    tygrysy: [:orly, :skunksy]
  }

  def calculate do
    @teams
    |> League.build(@played_matches)
    |> select_weeks_for_matches()
  end

  defp select_weeks_for_matches(league = %League{matches: matches}) do
    matches = Enum.reduce(matches, MapSet.new([]), fn match, acc ->
      week = select_week_for_match(match, acc)
      match = %Match{match | week: week}
      MapSet.put(acc, match)
    end)

    %League{league | matches: matches}
  end

  defp select_week_for_match(match = %Match{team1: team1, team2: team2, week: nil}, matches) do
    team1_weeks = select_weeks_already_played_by_team(team1, matches)
    team2_weeks = select_weeks_already_played_by_team(team2, matches)
    weeks = team1_weeks ++ team2_weeks

    weeks
    |> Enum.uniq()
    |> Enum.sort()
    |> find_week()
  end

  defp select_week_for_match(match = %Match{week: week}, _matches), do: week

  defp select_weeks_already_played_by_team(team, matches) when is_atom(team) do
    Enum.reduce(matches, [], fn %Match{team1: team1, team2: team2, week: week}, acc ->
      if (team1 == team || team2 == team) && !is_nil(week) do
        [week | acc]
      else
        acc
      end
    end)
  end

  defp find_week([]), do: @start_week
  defp find_week([week1, week2 | t]) when (week1 + 1) == week2, do: find_week([week2 | t])
  defp find_week([week1, week2 | t]) when (week1 + 1) != week2, do: week1 + 1
  defp find_week([week]), do: week + 1
end

Chapter1.Exercise1.calculate()