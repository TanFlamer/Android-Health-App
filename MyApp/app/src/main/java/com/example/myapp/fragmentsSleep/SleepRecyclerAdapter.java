package com.example.myapp.fragmentsSleep;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Sleep;

import java.util.HashMap;
import java.util.List;

public class SleepRecyclerAdapter extends RecyclerView.Adapter<SleepRecyclerAdapter.SleepRecyclerItemViewHolder> {

    Context context;
    List<Sleep> sleepList;
    HashMap<Sleep, Boolean> visibilityMap;

    public SleepRecyclerAdapter(Context context, List<Sleep> sleepList){
        this.context = context;
        this.sleepList = sleepList;
        visibilityMap = new HashMap<>();
        for(Sleep sleep : sleepList) visibilityMap.put(sleep, false);
    }

    @NonNull
    @Override
    public SleepRecyclerItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sleep_recycler_list_item, parent, false);
        return new SleepRecyclerItemViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SleepRecyclerItemViewHolder holder, int position) {
        Sleep sleep = sleepList.get(position);
        holder.titleView.setText(sleep.getDate().toString());
        holder.dateView.setText(sleep.getDate().toString());
        holder.sleepView.setText(sleep.getSleepTime().toString());
        holder.wakeView.setText(sleep.getWakeTime().toString());
        holder.durationView.setText(sleep.getSleepDuration().toString());
        holder.layoutHidden.setVisibility(Boolean.TRUE.equals(visibilityMap.get(sleep)) ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return sleepList.size();
    }

    public void updateSleepList(List<Sleep> newSleepList){
        sleepList.clear();
        sleepList.addAll(newSleepList);
        visibilityMap.clear();
        for(Sleep sleep : sleepList) visibilityMap.put(sleep, false);
    }

    public class SleepRecyclerItemViewHolder extends RecyclerView.ViewHolder {

        TextView titleView, dateView, sleepView, wakeView, durationView;
        LinearLayout layoutVisible, layoutHidden;

        public SleepRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sleepTitle);
            dateView = itemView.findViewById(R.id.sleepDate);
            sleepView = itemView.findViewById(R.id.sleepTime);
            wakeView = itemView.findViewById(R.id.wakeTime);
            durationView = itemView.findViewById(R.id.sleepDuration);

            layoutVisible = itemView.findViewById(R.id.sleepLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sleepLayoutHidden);

            layoutVisible.setOnClickListener(view -> {
                Sleep sleep = sleepList.get(getAdapterPosition());
                visibilityMap.put(sleep, Boolean.FALSE.equals(visibilityMap.get(sleep)));
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
