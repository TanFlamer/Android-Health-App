package com.example.myapp.fragmentsSleep.recyclerSleep;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;

import java.util.List;

public class SleepRecyclerAdapter extends RecyclerView.Adapter<SleepRecyclerAdapter.SleepItemViewHolder> {

    Context context;
    List<SleepRecyclerItem> sleepRecyclerItemList;

    public SleepRecyclerAdapter(Context context, List<SleepRecyclerItem> sleepRecyclerItemList){
        this.context = context;
        this.sleepRecyclerItemList = sleepRecyclerItemList;
    }

    @NonNull
    @Override
    public SleepItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sleep_recycler_list_item, parent, false);
        return new SleepItemViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SleepItemViewHolder holder, int position) {
        SleepRecyclerItem sleepRecyclerItem = sleepRecyclerItemList.get(position);

        holder.titleView.setText(sleepRecyclerItem.getTitle());
        holder.dateView.setText(sleepRecyclerItem.getDate());
        holder.sleepView.setText(sleepRecyclerItem.getSleepTime());
        holder.wakeView.setText(sleepRecyclerItem.getWakeTime());
        holder.durationView.setText(String.valueOf(sleepRecyclerItem.getSleepDuration()));

        boolean isHidden = sleepRecyclerItemList.get(position).isHidden();
        holder.layoutHidden.setVisibility(isHidden ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return sleepRecyclerItemList.size();
    }

    public class SleepItemViewHolder extends RecyclerView.ViewHolder {

        TextView titleView, dateView, sleepView, wakeView, durationView;
        LinearLayout layoutVisible, layoutHidden;

        public SleepItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sleepTitle);
            dateView = itemView.findViewById(R.id.sleepDate);
            sleepView = itemView.findViewById(R.id.sleepTime);
            wakeView = itemView.findViewById(R.id.wakeTime);
            durationView = itemView.findViewById(R.id.sleepDuration);

            layoutVisible = itemView.findViewById(R.id.sleepLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sleepLayoutHidden);

            layoutVisible.setOnClickListener(view -> {
                SleepRecyclerItem sleepRecyclerItem = sleepRecyclerItemList.get(getAdapterPosition());
                sleepRecyclerItem.setHidden(!sleepRecyclerItem.isHidden());
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
