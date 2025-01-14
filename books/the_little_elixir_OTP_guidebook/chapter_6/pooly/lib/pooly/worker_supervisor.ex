defmodule Pooly.WorkerSupervisor do
  @moduledoc false

  use Supervisor

  #########
  #  API  #
  #########

  def start_link({_, _, _} = mfa) do
    Supervisor.start_link(__MODULE__, mfa)
  end

  #############
  # Callbacks #
  #############

  def init({m, f, a} = _mfa) do
    worker_opts = [restart: :permanent, function: f]
    children = [worker(m, a, worker_opts)]
    opts = [strategy: :simple_one_for_one, max_restarts: 5, max_seconds: 5]

    Supervisor.init(children, opts)
  end
end
